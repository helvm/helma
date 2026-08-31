{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module HelVM.HelMA.Automata.Piet.LLVM.Parser
  ( ParserError (..)
  , parse
  , parseFilledImage
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Filler
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Position
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.WhiteCodelSlider
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

import           HelVM.HelMA.Automata.Piet.Types.Coordinates              ( Coordinates )

import           Control.Arrow                                            ( Arrow ((***)) )
import           Control.Monad.Except

import qualified Data.Foldable1                                           as F1
import qualified Data.IntMap                                              as IM
import qualified Data.IntSet                                              as IS
import qualified Data.List.NonEmpty                                       as NE
import qualified Data.Map                                                 as M
import           Data.MonoTraversable
import           Data.Vector                                              ( Vector )
import qualified Data.Vector                                              as V

type CodelTable = Vector (Vector (Codel, Int))
type BlockTable = IntMap [Coordinates]

data ParserError
  = EmptyBlockTableError -- ^ The block table is empty.
  | IllegalInitialColorError -- ^ The initial codel of the block table is black.
  | MissingCodelIndexError Int -- ^ A codel index in the codel table is missing.
  deriving stock (Eq, Show)

parse ∷ MonadError ParserError m ⇒ Vector (Vector Codel) → m SyntaxGraph
parse image = parseFilledImage (V.zipWith V.zip image indices, positionTable)
  where
    (indices, positionTable) = fillAll image

parseFilledImage ∷ MonadError ParserError m ⇒ (CodelTable, BlockTable) → m SyntaxGraph
parseFilledImage (codelTable, blockTable) = do
  initial <- searchInitialBlock codelTable
  parseFrom codelTable blockTable initial

parseFrom ∷ MonadError ParserError m ⇒ CodelTable → BlockTable → Maybe (Int, DPCC) → m SyntaxGraph
parseFrom _ _ Nothing = pure EmptySyntaxGraph
parseFrom codelTable blockTable (Just (initialBlockIndex, initialDPCC')) = do
  blockMap <- execStateT (parseState codelTable blockTable initialBlockIndex) IM.empty
  pure $ SyntaxGraph initialBlockIndex initialDPCC' blockMap

parseState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ CodelTable → BlockTable → Int → m ()
parseState codelTable blockTable blockIndex = do
  blockCoords <- justOrThrow (MissingCodelIndexError blockIndex) $ blockTable IM.!? blockIndex
  let blockSize = olength blockCoords
  let nextBlockList = mapMaybe (\(dpcc, pos) -> (dpcc,) <$> searchNextBlock codelTable pos dpcc blockSize) $ minMaxCoords blockCoords
  let block = Block $ M.fromList nextBlockList
  modify $ IM.insert blockIndex block

  visitedIndices <- IM.keysSet <$> get
  let nextBlockIndices = mapMaybe (nextBlockToIndex . snd) nextBlockList
  let unvisitedBlockIndices = filter (`IS.notMember` visitedIndices) nextBlockIndices
  mapM_ (parseState codelTable blockTable) unvisitedBlockIndices

searchInitialBlock ∷ MonadError ParserError m ⇒ CodelTable → m (Maybe (Int, DPCC))
searchInitialBlock codelTable = do
  (initialCodel, initialBlockIndex) <- justOrThrow EmptyBlockTableError $ codelTable V.!? 0 >>= (V.!? 0)
  processInitial codelTable initialCodel initialBlockIndex

processInitial ∷ MonadError ParserError m ⇒ CodelTable → Codel → Int → m (Maybe (Int, DPCC))
processInitial _ (AchromaticCodel _ _) blockIdx = pure $ Just (blockIdx, initialDPCC)
processInitial codelTable WhiteCodel _          = pure $ nextBlockToIndexAndDPCC $ slideOnWhiteBlock codelTable (0, 0) initialDPCC
processInitial _ BlackCodel          _          = throwError IllegalInitialColorError

initialDPCC ∷ DPCC
initialDPCC = DPCC DPRight CCLeft

searchNextBlock ∷ CodelTable → Coordinates → DPCC → Int → Maybe NextBlock
searchNextBlock codelTable (x, y) dpcc@(DPCC dp _) blockSize = do
  (AchromaticCodel currentHue currentLightness, _) <- codelTable V.!? y >>= (V.!? x)
  let nextPos@(nextX, nextY) = move dp (x, y)
  (nextCodel, blockIndex) <- codelTable V.!? nextY >>= (V.!? nextX)
  processNextCodel codelTable nextCodel currentHue currentLightness nextPos dpcc blockSize blockIndex

processNextCodel ∷ CodelTable → Codel → Hue → Lightness → Coordinates → DPCC → Int → Int → Maybe NextBlock
processNextCodel _ (AchromaticCodel nextHue nextLightness) curHue curLight _ dpcc blockSize blockIdx =
  Just $ NextBlock (commandFromTransition (curHue, curLight) (nextHue, nextLightness) blockSize) dpcc blockIdx
processNextCodel codelTable WhiteCodel _ _ pos dpcc _ _ =
  Just $ slideOnWhiteBlock codelTable pos dpcc
processNextCodel _ BlackCodel _ _ _ _ _ _ =
  Nothing

nextBlockToIndex ∷ NextBlock → Maybe Int
nextBlockToIndex (NextBlock _ _ nextBlockIndex) = Just nextBlockIndex
nextBlockToIndex ExitProgram                    = Nothing

nextBlockToIndexAndDPCC ∷ NextBlock → Maybe (Int, DPCC)
nextBlockToIndexAndDPCC (NextBlock _ nextBlockDPCC nextBlockIndex) = Just (nextBlockIndex, nextBlockDPCC)
nextBlockToIndexAndDPCC ExitProgram                                = Nothing

minMaxCoords ∷ [Coordinates] → [(DPCC, Coordinates)]
minMaxCoords positions = processPositions (nonEmpty positions)
  where
    processPositions (Just nePositions) = fmap (`maximumOn` nePositions) <$> fs
    processPositions Nothing            = []

fs ∷ [(DPCC, Coordinates → Coordinates)]
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
