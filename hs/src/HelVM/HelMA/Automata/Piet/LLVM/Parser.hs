{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module HelVM.HelMA.Automata.Piet.LLVM.Parser
  ( ParserError (..)
  , parse
  , parseFilledImage
  ) where

import           HelVM.HelMA.Automata.Piet.Filler
import           HelVM.HelMA.Automata.Piet.LLVM.WhiteCodelSlider
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Control.Arrow                                    ( Arrow ((***)) )
import           Control.Monad.Except

import qualified Data.Foldable1                                   as F1
import qualified Data.IntMap                                      as IM
import qualified Data.IntSet                                      as IS
import qualified Data.List.NonEmpty                               as NE
import qualified Data.Map                                         as M
import           Data.MonoTraversable
import           Data.Vector                                      ( Vector )
import qualified Data.Vector                                      as V

type Matrix a   = Vector (Vector a)
type CodelTable = Matrix (Color, Int)
type BlockTable = IntMap BlockCoordinates

data ParserError
  = EmptyBlockTableError
  | IllegalInitialColorError
  | MissingCodelIndexError Int
  deriving stock (Eq, Show)

parse ∷ MonadError ParserError m ⇒ Matrix Color → m SyntaxGraphMaybe
parse image = parseFilledImageWithSplit (fillAll image) image

parseFilledImageWithSplit ∷ MonadError ParserError m ⇒ (Matrix Int, BlockTable) → Matrix Color → m SyntaxGraphMaybe
parseFilledImageWithSplit (indices, positionTable) image = parseFilledImage (V.zipWith V.zip image indices, positionTable)

parseFilledImage ∷ MonadError ParserError m ⇒ (CodelTable, BlockTable) → m SyntaxGraphMaybe
parseFilledImage (codelTable, blockTable) = parseFrom codelTable blockTable =<< searchInitialBlock codelTable

parseFrom ∷ MonadError ParserError m ⇒ CodelTable → BlockTable → Maybe (Int, Course) → m SyntaxGraphMaybe
parseFrom _ _ Nothing                                         = pure Nothing
parseFrom codelTable blockTable (Just (initialBlockIndex, initialCourse')) = fmap (Just . SyntaxGraph initialBlockIndex initialCourse') (execStateT (parseState codelTable blockTable initialBlockIndex) IM.empty)

parseState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → Int → m ()
parseState codelTable blockTable blockIndex = processBlockState codelTable blockTable blockIndex =<< justOrThrow (MissingCodelIndexError blockIndex) (blockTable IM.!? blockIndex)

processBlockState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → Int → BlockCoordinates → m ()
processBlockState codelTable blockTable blockIndex blockCoords = insertBlock blockIndex (buildNextBlockList codelTable blockCoords) >> processUnvisited codelTable blockTable (buildNextBlockList codelTable blockCoords)

insertBlock ∷ MonadState (IntMap Block) m ⇒ Int → [(Course, NextBlockMaybe)] → m ()
insertBlock blockIndex nextBlockList = modify (IM.insert blockIndex (Block $ M.fromList nextBlockList))

processUnvisited ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → [(Course, NextBlockMaybe)] → m ()
processUnvisited codelTable blockTable nextBlockList = traverse_ (parseState codelTable blockTable) . filterUnvisited nextBlockList =<< get

filterUnvisited ∷ [(Course, NextBlockMaybe)] → IntMap Block → [Int]
filterUnvisited nextBlockList visitedMap = filter (`IS.notMember` IM.keysSet visitedMap) (mapMaybe (nextBlockToIndex . snd) nextBlockList)

buildNextBlockList ∷ CodelTable → BlockCoordinates → [(Course, NextBlockMaybe)]
buildNextBlockList codelTable blockCoords = mapMaybe (findCourseNextBlock codelTable blockCoords (olength blockCoords)) (minMaxCoords blockCoords)

findCourseNextBlock ∷ CodelTable → BlockCoordinates → Int → (Course, Coordinates) → Maybe (Course, NextBlockMaybe)
findCourseNextBlock codelTable _ blockSize (course, pos) = fmap (course,) (searchNextBlock codelTable pos course blockSize)

searchInitialBlock ∷ MonadError ParserError m ⇒ CodelTable → m (Maybe (Int, Course))
searchInitialBlock codelTable = uncurry (processInitial codelTable) =<< justOrThrow EmptyBlockTableError (codelTable V.!? 0 >>= (V.!? 0))

processInitial ∷ MonadError ParserError m ⇒ CodelTable → Color → Int → m (Maybe (Int, Course))
processInitial _ (Chromatic (ChromaticColor _ _)) blockIdx = pure $ Just (blockIdx, initialCourse)
processInitial codelTable White _                          = pure $ nextBlockToIndexAndCourse $ slideOnWhiteBlock codelTable (0, 0) initialCourse
processInitial _ Black          _                          = throwError IllegalInitialColorError

searchNextBlock ∷ CodelTable → Coordinates → Course → Int → Maybe NextBlockMaybe
searchNextBlock codelTable (x, y) course blockSize = searchNextBlockWithColor codelTable (x, y) course blockSize =<< (codelTable V.!? y >>= (V.!? x))

searchNextBlockWithColor ∷ CodelTable → Coordinates → Course → Int → (Color, Int) → Maybe NextBlockMaybe
searchNextBlockWithColor codelTable (x, y) course@(Course dp _) blockSize (Chromatic (ChromaticColor currentHue currentLightness), _) = searchNextBlockFromMove codelTable (move dp (x, y)) course blockSize currentHue currentLightness =<< fetchNextCodel codelTable (move dp (x, y))
searchNextBlockWithColor _ _ _ _ _                                                                                                     = Nothing

fetchNextCodel ∷ CodelTable → Coordinates → Maybe (Color, Int)
fetchNextCodel codelTable (nextX, nextY) = codelTable V.!? nextY >>= (V.!? nextX)

searchNextBlockFromMove ∷ CodelTable → Coordinates → Course → Int → Hue → Lightness → (Color, Int) → Maybe NextBlockMaybe
searchNextBlockFromMove codelTable nextPos course blockSize curHue curLight (nextCodel, blockIndex) = processNextCodel codelTable nextCodel curHue curLight nextPos course blockSize blockIndex

processNextCodel ∷ CodelTable → Color → Hue → Lightness → Coordinates → Course → Int → Int → Maybe NextBlockMaybe
processNextCodel _ (Chromatic (ChromaticColor nextHue nextLightness)) curHue curLight _ course blockSize blockIdx = Just $ Just $ NextBlock (commandFromTransition (curHue, curLight) (nextHue, nextLightness) blockSize) course blockIdx
processNextCodel codelTable White _ _ pos course _ _                                                              = Just $ slideOnWhiteBlock codelTable pos course
processNextCodel _ Black _ _ _ _ _ _                                                                             = Nothing

nextBlockToIndex ∷ NextBlockMaybe → Maybe Int
nextBlockToIndex (Just (NextBlock _ _ nextBlockIndex)) = Just nextBlockIndex
nextBlockToIndex Nothing                               = Nothing

nextBlockToIndexAndCourse ∷ NextBlockMaybe → Maybe (Int, Course)
nextBlockToIndexAndCourse (Just (NextBlock _ nextBlockCourse nextBlockIndex)) = Just (nextBlockIndex, nextBlockCourse)
nextBlockToIndexAndCourse Nothing                                             = Nothing

minMaxCoords ∷ BlockCoordinates → [(Course, Coordinates)]
minMaxCoords positions = processPositions (nonEmpty positions)

processPositions ∷ Maybe (NE.NonEmpty Coordinates) → [(Course, Coordinates)]
processPositions (Just nePositions) = fmap (`maximumOn` nePositions) <$> fs
processPositions Nothing            = []

fs ∷ [(Course, Coordinates → Coordinates)]
fs = [ (Course DPRight CCLeft,  second negate)
     , (Course DPRight CCRight, id)
     , (Course DPDown  CCLeft,  swap)
     , (Course DPDown  CCRight, second negate . swap)
     , (Course DPLeft  CCLeft,  first negate)
     , (Course DPLeft  CCRight, negate *** negate)
     , (Course DPUp    CCLeft,  (negate *** negate) . swap)
     , (Course DPUp    CCRight, first negate . swap)
     ]

maximumOn ∷ Ord b ⇒ (a → b) → NE.NonEmpty a → a
maximumOn f = F1.maximumBy (comparing f)

justOrThrow ∷ MonadError e m ⇒ e → Maybe a → m a
justOrThrow e = maybe (throwError e) pure
