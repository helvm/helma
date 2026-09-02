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

type CodelTable = Vector (Vector (Color, Int))
type BlockTable = IntMap BlockCoordinates

data ParserError
  = EmptyBlockTableError -- ^ The block table is empty.
  | IllegalInitialColorError -- ^ The initial codel of the block table is black.
  | MissingCodelIndexError Int -- ^ A codel index in the codel table is missing.
  deriving stock (Eq, Show)

parse ∷ MonadError ParserError m ⇒ Vector (Vector Color) → m SyntaxGraphMaybe
parse image = parseFilledImage (V.zipWith V.zip image indices, positionTable)
  where
    (indices, positionTable) = fillAll image

parseFilledImage ∷ MonadError ParserError m ⇒ (CodelTable, BlockTable) → m SyntaxGraphMaybe
parseFilledImage (codelTable, blockTable) = do
  initial <- searchInitialBlock codelTable
  parseFrom codelTable blockTable initial

parseFrom ∷ MonadError ParserError m ⇒ CodelTable → BlockTable → Maybe (Int, Course) → m SyntaxGraphMaybe
parseFrom _ _ Nothing = pure Nothing
parseFrom codelTable blockTable (Just (initialBlockIndex, initialCourse')) = do
  blockMap <- execStateT (parseState codelTable blockTable initialBlockIndex) IM.empty
  pure $ Just $ SyntaxGraph initialBlockIndex initialCourse' blockMap

parseState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → Int → m ()
parseState codelTable blockTable blockIndex = do
  blockCoords <- justOrThrow (MissingCodelIndexError blockIndex) $ blockTable IM.!? blockIndex
  let blockSize = olength blockCoords
  let nextBlockList = mapMaybe (\(course, pos) -> (course,) <$> searchNextBlock codelTable pos course blockSize) $ minMaxCoords blockCoords
  let block = Block $ M.fromList nextBlockList
  modify $ IM.insert blockIndex block

  visitedIndices <- IM.keysSet <$> get
  let nextBlockIndices = mapMaybe (nextBlockToIndex . snd) nextBlockList
  let unvisitedBlockIndices = filter (`IS.notMember` visitedIndices) nextBlockIndices
  mapM_ (parseState codelTable blockTable) unvisitedBlockIndices

searchInitialBlock ∷ MonadError ParserError m ⇒ CodelTable → m (Maybe (Int, Course))
searchInitialBlock codelTable = do
  (initialCodel, initialBlockIndex) <- justOrThrow EmptyBlockTableError $ codelTable V.!? 0 >>= (V.!? 0)
  processInitial codelTable initialCodel initialBlockIndex

processInitial ∷ MonadError ParserError m ⇒ CodelTable → Color → Int → m (Maybe (Int, Course))
processInitial _ (Chromatic (ChromaticColor _ _)) blockIdx = pure $ Just (blockIdx, initialCourse)
processInitial codelTable White _                          = pure $ nextBlockToIndexAndCourse $ slideOnWhiteBlock codelTable (0, 0) initialCourse
processInitial _ Black          _                          = throwError IllegalInitialColorError

searchNextBlock ∷ CodelTable → Coordinates → Course → Int → Maybe NextBlockMaybe
searchNextBlock codelTable (x, y) course@(Course dp _) blockSize = do
  (Chromatic (ChromaticColor currentHue currentLightness), _) <- codelTable V.!? y >>= (V.!? x)
  let nextPos@(nextX, nextY) = move dp (x, y)
  (nextCodel, blockIndex) <- codelTable V.!? nextY >>= (V.!? nextX)
  processNextCodel codelTable nextCodel currentHue currentLightness nextPos course blockSize blockIndex

processNextCodel ∷ CodelTable → Color → Hue → Lightness → Coordinates → Course → Int → Int → Maybe NextBlockMaybe
processNextCodel _ (Chromatic (ChromaticColor nextHue nextLightness)) curHue curLight _ course blockSize blockIdx =
  Just $ Just $ NextBlock (commandFromTransition (curHue, curLight) (nextHue, nextLightness) blockSize) course blockIdx
processNextCodel codelTable White _ _ pos course _ _ =
  Just $ slideOnWhiteBlock codelTable pos course
processNextCodel _ Black _ _ _ _ _ _ =
  Nothing

nextBlockToIndex ∷ NextBlockMaybe → Maybe Int
nextBlockToIndex (Just (NextBlock _ _ nextBlockIndex)) = Just nextBlockIndex
nextBlockToIndex Nothing                               = Nothing

nextBlockToIndexAndCourse ∷ NextBlockMaybe → Maybe (Int, Course)
nextBlockToIndexAndCourse (Just (NextBlock _ nextBlockCourse nextBlockIndex)) = Just (nextBlockIndex, nextBlockCourse)
nextBlockToIndexAndCourse Nothing                                             = Nothing

minMaxCoords ∷ BlockCoordinates → [(Course, Coordinates)]
minMaxCoords positions = processPositions (nonEmpty positions)
  where
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
