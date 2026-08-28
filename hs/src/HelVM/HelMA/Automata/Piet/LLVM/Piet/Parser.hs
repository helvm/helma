{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module HelVM.HelMA.Automata.Piet.LLVM.Piet.Parser
  ( ParserError (..)
  , parse
  , parseFilledImage
  ) where

import           Control.Arrow                                                 ( Arrow ((***)) )
import           Control.Monad.Except
import qualified Data.Foldable1                                                as F1
import qualified Data.IntMap                                                   as IM
import qualified Data.IntSet                                                   as IS
import qualified Data.List.NonEmpty                                            as NE
import qualified Data.Map                                                      as M
import           Data.Vector                                                   ( Vector )
import qualified Data.Vector                                                   as V
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Filler
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Position
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.WhiteCodelSlider
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax

import           Data.MonoTraversable

data ParserError
  = EmptyBlockTableError -- ^ The block table is empty.
  | IllegalInitialColorError -- ^ The initial codel of the block table is black.
  | MissingCodelIndexError Int -- ^ A codel index in the codel table is missing.
  deriving stock (Eq, Show)

parse ∷ MonadError ParserError m ⇒ Vector (Vector Codel) → m SyntaxGraph
parse image = parseFilledImage (V.zipWith V.zip image indices, positionTable)
  where
    (indices, positionTable) = fillAll image

parseFilledImage ∷ MonadError ParserError m ⇒ (Vector (Vector (Codel, Int)), IntMap [(Int, Int)]) → m SyntaxGraph
parseFilledImage (codelTable, blockTable) = searchInitialBlock >>= parseFrom where
  parseFrom ∷ MonadError ParserError m ⇒ Maybe (Int, DPCC) → m SyntaxGraph
  parseFrom Nothing = pure EmptySyntaxGraph
  parseFrom (Just (initialBlockIndex, initialDPCC)) = do
    blockMap <- execStateT (parseState initialBlockIndex) IM.empty
    pure $ SyntaxGraph initialBlockIndex initialDPCC blockMap

  parseState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ Int → m ()
  parseState blockIndex = do
    blockCoords <- justOrThrow (MissingCodelIndexError blockIndex) $ blockTable IM.!? blockIndex
    let blockSize = olength blockCoords
    let nextBlockList = mapMaybe (\(dpcc, pos) -> (dpcc,) <$> searchNextBlock pos dpcc blockSize) $ minMaxCoords blockCoords
    let block = Block $ M.fromList nextBlockList
    modify $ IM.insert blockIndex block

    visitedIndices <- IM.keysSet <$> get
    let nextBlockIndices = mapMaybe (nextBlockToIndex . snd) nextBlockList
    let unvisitedBlockIndices = filter (`IS.notMember` visitedIndices) nextBlockIndices
    mapM_ parseState unvisitedBlockIndices

  nextBlockToIndex ∷ NextBlock → Maybe Int
  nextBlockToIndex (NextBlock _ _ nextBlockIndex) = Just nextBlockIndex
  nextBlockToIndex ExitProgram                    = Nothing

  nextBlockToIndexAndDPCC ∷ NextBlock → Maybe (Int, DPCC)
  nextBlockToIndexAndDPCC (NextBlock _ nextBlockDPCC nextBlockIndex) = Just (nextBlockIndex, nextBlockDPCC)
  nextBlockToIndexAndDPCC ExitProgram                                = Nothing

  searchInitialBlock ∷ MonadError ParserError m ⇒ m (Maybe (Int, DPCC))
  searchInitialBlock = do
    (initialCodel, initialBlockIndex) <- justOrThrow EmptyBlockTableError $ codelTable V.!? 0 >>= (V.!? 0)
    processInitial initialCodel initialBlockIndex
    where
      initialDPCC = DPCC DPRight CCLeft
      processInitial (AchromaticCodel _ _) blockIdx = pure $ Just (blockIdx, initialDPCC)
      processInitial WhiteCodel          _          = pure $ nextBlockToIndexAndDPCC $ slideOnWhiteBlock codelTable (0, 0) initialDPCC
      processInitial BlackCodel          _          = throwError IllegalInitialColorError

  searchNextBlock ∷ (Int, Int) → DPCC → Int → Maybe NextBlock
  searchNextBlock (x, y) dpcc@(DPCC dp _) blockSize = do
    (AchromaticCodel currentHue currentLightness, _) <- codelTable V.!? y >>= (V.!? x)
    let nextPos@(nextX, nextY) = move dp (x, y)
    (nextCodel, blockIndex) <- codelTable V.!? nextY >>= (V.!? nextX)
    processNextCodel nextCodel currentHue currentLightness nextPos blockIndex
    where
      processNextCodel (AchromaticCodel nextHue nextLightness) curHue curLight _ blockIdx =
        Just $ NextBlock (commandFromTransition (curHue, curLight) (nextHue, nextLightness) blockSize) dpcc blockIdx
      processNextCodel WhiteCodel _ _ pos _ =
        Just $ slideOnWhiteBlock codelTable pos dpcc
      processNextCodel BlackCodel _ _ _ _ =
        Nothing

minMaxCoords ∷ [(Int, Int)] → [(DPCC, (Int, Int))]
minMaxCoords positions = processPositions (nonEmpty positions)
  where
    processPositions (Just nePositions) = fmap (`maximumOn` nePositions) <$> fs
    processPositions Nothing            = []

fs ∷ [(DPCC, (Int, Int) → (Int, Int))]
fs = [ (DPCC DPRight CCLeft,  second negate)
     , (DPCC DPRight CCRight, id)
     , (DPCC DPDown  CCLeft,  swap)
     , (DPCC DPDown  CCRight, second negate . swap)
     , (DPCC DPLeft  CCLeft,  first negate)
     , (DPCC DPLeft  CCRight, negate *** negate)
     , (DPCC DPUp    CCLeft,  (negate *** negate) . swap)
     , (DPCC DPUp    CCRight, first negate . swap)
     ]

maximumOn ∷ Ord b ⇒ (a → b) → NE.NonEmpty a → a
maximumOn f = F1.maximumBy (comparing f)

justOrThrow ∷ MonadError e m ⇒ e → Maybe a → m a
justOrThrow e = maybe (throwError e) pure
