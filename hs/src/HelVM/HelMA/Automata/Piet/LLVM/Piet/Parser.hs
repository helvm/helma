{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | Functions to parse images.
module HelVM.HelMA.Automata.Piet.LLVM.Piet.Parser
  ( ParserError (..)
  , parse
  , parseFilledImage
  ) where

import           Control.Arrow
import           Control.Monad.Except
-- import Control.Monad.State
-- import           Data.IntMap                                                   ( IntMap )
import qualified Data.IntMap                                                   as IM
import qualified Data.IntSet                                                   as IS
import           Data.List
import qualified Data.Map                                                      as M
-- import           Data.Maybe
-- import           Data.Tuple
import           Data.Vector                                                   ( Vector )
import qualified Data.Vector                                                   as V
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Filler
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Position
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.WhiteCodelSlider
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax

data ParserError
  = EmptyBlockTableError -- ^ The block table is empty.
  | IllegalInitialColorError -- ^ The initial codel of the block table is black.
  | MissingCodelIndexError Int -- ^ A codel index in the codel table is missing.
  deriving stock (Eq, Show)

-- | Parse codels into a 'SyntaxGraph'.
parse ∷ MonadError ParserError m ⇒ Vector (Vector Codel) → m SyntaxGraph
parse image = let (indices, positionTable) = fillAll image in parseFilledImage (V.zipWith V.zip image indices, positionTable)

-- | Parse a filled image which is returned by 'fillAll' into a 'SyntaxGraph'.
parseFilledImage ∷ MonadError ParserError m ⇒ (Vector (Vector (Codel, Int)), IntMap [(Int, Int)]) → m SyntaxGraph
parseFilledImage (codelTable, blockTable) = searchInitialBlock >>= parseFrom where
  parseFrom ∷ MonadError ParserError m ⇒ Maybe (Int, DPCC) → m SyntaxGraph
  parseFrom Nothing = return EmptySyntaxGraph
  parseFrom (Just (initialBlockIndex, initialDPCC)) = do
    blockMap <- execStateT (parseState initialBlockIndex) IM.empty
    return $ SyntaxGraph initialBlockIndex initialDPCC blockMap

  parseState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ Int → m ()
  parseState blockIndex = do
    blockCoords <- justOrThrow (MissingCodelIndexError blockIndex) $ blockTable IM.!? blockIndex

    let blockSize = length blockCoords
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
    let initialDPCC = DPCC DPRight CCLeft
    case initialCodel of
      AchromaticCodel _ _ -> return $ Just (initialBlockIndex, initialDPCC)
      WhiteCodel          -> return $ nextBlockToIndexAndDPCC $ slideOnWhiteBlock codelTable (0, 0) initialDPCC
      BlackCodel          -> throwError IllegalInitialColorError

  searchNextBlock ∷ (Int, Int) → DPCC → Int → Maybe NextBlock
  searchNextBlock (x, y) dpcc@(DPCC dp _) blockSize = do
    (AchromaticCodel currentHue currentLightness, _) <- codelTable V.!? y >>= (V.!? x)
    let (nextX, nextY) = move dp (x, y)
    (nextCodel, blockIndex) <- codelTable V.!? nextY >>= (V.!? nextX)
    case nextCodel of
      AchromaticCodel nextHue nextLightness ->
        let command = commandFromTransition (currentHue, currentLightness) (nextHue, nextLightness) blockSize
        in Just $ NextBlock command dpcc blockIndex
      WhiteCodel -> Just $ slideOnWhiteBlock codelTable (nextX, nextY) dpcc
      BlackCodel -> Nothing

-- {-# ANN minMaxCoords "HLint: ignore Redundant id" #-}
-- {-# ANN minMaxCoords "HLint: ignore Use first" #-}
-- {-# ANN minMaxCoords "HLint: ignore Use second" #-}
minMaxCoords ∷ [(Int, Int)] → [(DPCC, (Int, Int))]
minMaxCoords positions = fmap (`maximumOn` positions) <$> fs where
  fs = [ (DPCC DPRight CCLeft,  (id     *** negate) . id  )
       , (DPCC DPRight CCRight, (id     *** id    ) . id  )
       , (DPCC DPDown  CCLeft,  (id     *** id    ) . swap)
       , (DPCC DPDown  CCRight, (id     *** negate) . swap)
       , (DPCC DPLeft  CCLeft,  (negate *** id    ) . id  )
       , (DPCC DPLeft  CCRight, (negate *** negate) . id  )
       , (DPCC DPUp    CCLeft,  (negate *** negate) . swap)
       , (DPCC DPUp    CCRight, (negate *** id    ) . swap)
       ]

maximumOn ∷ Ord b ⇒ (a → b) → [a] → a
maximumOn f = maximumBy (\x y -> compare (f x) (f y))

justOrThrow ∷ MonadError e m ⇒ e → Maybe a → m a
justOrThrow e = maybe (throwError e) return
