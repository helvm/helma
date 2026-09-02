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

import           Relude.Extra

type Matrix a   = Vector (Vector a)
type CodelTable = Matrix (Color, Int)
type BlockTable = IntMap BlockCoordinates

data ParserError
  = EmptyBlockTableError
  | IllegalInitialColorError
  | MissingCodelIndexError Int
  deriving stock (Eq, Show)

parse ∷ MonadError ParserError m ⇒ Matrix Color → m (Maybe SyntaxGraph)
parse image = parseFilledImageWithSplit (fillAll image) image

parseFilledImageWithSplit ∷ MonadError ParserError m ⇒ (Matrix Int, BlockTable) → Matrix Color → m (Maybe SyntaxGraph)
parseFilledImageWithSplit (indices, positionTable) image = parseFilledImage (V.zipWith V.zip image indices, positionTable)

parseFilledImage ∷ MonadError ParserError m ⇒ (CodelTable, BlockTable) → m (Maybe SyntaxGraph)
parseFilledImage (codelTable, blockTable) = parseFrom codelTable blockTable =<< searchInitialBlock codelTable

parseFrom ∷ MonadError ParserError m ⇒ CodelTable → BlockTable → Maybe (Int, Course) → m (Maybe SyntaxGraph)
parseFrom _ _ Nothing                                          = pure Nothing
parseFrom codelTable blockTable (Just (entryIndex, entryCourse')) =
  Just . SyntaxGraph entryIndex entryCourse' <$> execStateT (parseState codelTable blockTable entryIndex) IM.empty

parseState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → Int → m ()
parseState codelTable blockTable blockIndex =
  processBlockState codelTable blockTable blockIndex =<< justOrThrow (MissingCodelIndexError blockIndex) (blockTable IM.!? blockIndex)

processBlockState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → Int → BlockCoordinates → m ()
processBlockState codelTable blockTable blockIndex blockCoords = do
  let nextBlockList = buildNextBlockList codelTable blockCoords
  insertBlock blockIndex nextBlockList
  processUnvisited codelTable blockTable nextBlockList

insertBlock ∷ MonadState (IntMap Block) m ⇒ Int → [(Course, Maybe NextBlock)] → m ()
insertBlock blockIndex nextBlockList = modify (IM.insert blockIndex (Block $ M.fromList nextBlockList))

processUnvisited ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → [(Course, Maybe NextBlock)] → m ()
processUnvisited codelTable blockTable nextBlockList = traverse_ (parseState codelTable blockTable) . filterUnvisited nextBlockList =<< get

filterUnvisited ∷ [(Course, Maybe NextBlock)] → IntMap Block → [Int]
filterUnvisited nextBlockList visitedMap = filter (`IS.notMember` IM.keysSet visitedMap) (mapMaybe (nextBlockToIndex . snd) nextBlockList)

buildNextBlockList ∷ CodelTable → BlockCoordinates → [(Course, Maybe NextBlock)]
buildNextBlockList codelTable blockCoords = mapMaybe (findCourseNextBlock codelTable blockCoords (olength blockCoords)) (minMaxCoords blockCoords)

findCourseNextBlock ∷ CodelTable → BlockCoordinates → Int → (Course, Coordinates) → Maybe (Course, Maybe NextBlock)
findCourseNextBlock codelTable _ blockSize (course, pos) = (course,) <$> searchNextBlock codelTable pos course blockSize

searchInitialBlock ∷ MonadError ParserError m ⇒ CodelTable → m (Maybe (Int, Course))
searchInitialBlock codelTable = uncurry (processInitial codelTable) =<< justOrThrow EmptyBlockTableError (codelTable V.!? 0 >>= (V.!? 0))

processInitial ∷ MonadError ParserError m ⇒ CodelTable → Color → Int → m (Maybe (Int, Course))
processInitial _ (Chromatic (ChromaticColor _ _)) blockIdx = pure $ Just (blockIdx, initialCourse)
processInitial codelTable White _                          = pure $ nextBlockToIndexAndCourse $ slideOnWhiteBlock codelTable (0, 0) initialCourse
processInitial _ Black          _                          = throwError IllegalInitialColorError

searchNextBlock ∷ CodelTable → Coordinates → Course → Int → Maybe (Maybe NextBlock)
searchNextBlock codelTable (x, y) course blockSize = searchNextBlockWithColor codelTable (x, y) course blockSize =<< (codelTable V.!? y >>= (V.!? x))

searchNextBlockWithColor ∷ CodelTable → Coordinates → Course → Int → (Color, Int) → Maybe (Maybe NextBlock)
searchNextBlockWithColor codelTable (x, y) course@(Course dp _) blockSize (Chromatic (ChromaticColor currentHue currentLightness), _) =
  searchNextBlockFromMove codelTable (move dp (x, y)) course blockSize currentHue currentLightness =<< fetchNextCodel codelTable (move dp (x, y))
searchNextBlockWithColor _ _ _ _ _                                                                                                     = Nothing

fetchNextCodel ∷ CodelTable → Coordinates → Maybe (Color, Int)
fetchNextCodel codelTable (nextX, nextY) = codelTable V.!? nextY >>= (V.!? nextX)

searchNextBlockFromMove ∷ CodelTable → Coordinates → Course → Int → Hue → Lightness → (Color, Int) → Maybe (Maybe NextBlock)
searchNextBlockFromMove codelTable nextPos course blockSize curHue curLight (nextCodel, blockIndex) =
  processNextCodel codelTable nextCodel curHue curLight nextPos course blockSize blockIndex

processNextCodel ∷ CodelTable → Color → Hue → Lightness → Coordinates → Course → Int → Int → Maybe (Maybe NextBlock)
processNextCodel _ (Chromatic (ChromaticColor nextHue nextLightness)) curHue curLight _ course blockSize blockIdx =
  Just $ Just $ NextBlock (commandFromTransition (curHue, curLight) (nextHue, nextLightness) blockSize) course blockIdx
processNextCodel codelTable White _ _ pos course _ _ = Just $ slideOnWhiteBlock codelTable pos course
processNextCodel _ Black _ _ _ _ _ _                 = Nothing

nextBlockToIndex ∷ Maybe NextBlock → Maybe Int
nextBlockToIndex nb = view blockIndexL <$> nb

nextBlockToIndexAndCourse ∷ Maybe NextBlock → Maybe (Int, Course)
nextBlockToIndexAndCourse nb = (,) <$> (view blockIndexL <$> nb) <*> (view courseL <$> nb)

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
