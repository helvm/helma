module HelVM.HelMA.Automata.Piet.Filler
  ( fillAll
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Grid

import           Control.Monad.ST                            ( ST, runST )

import qualified Data.IntMap.Strict                          as IM
import qualified Data.Vector                                 as V
import qualified Data.Vector.Unboxed                         as UV
import qualified Data.Vector.Unboxed.Mutable                 as UMV

-- TYPES

type FillState s = (UMV.MVector s Int, IntMap BlockCoordinates)
type FillResult = (Grid Int, IntMap BlockCoordinates)

type LoopFill s = Coordinates → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)
type LoopStack s = BlockCoordinates → BlockCoordinates → ST s BlockCoordinates

-- PUBLIC API

fillAll ∷ Eq a ⇒ Grid a → FillResult
fillAll grid = runST $ fillST grid

-- PRIVATE HELPERS (TOP-DOWN)

fillST ∷ Eq a ⇒ Grid a → ST s FillResult
fillST grid
  | grid.widthGrid == 0 || grid.heightGrid == 0 = pure (Grid grid.widthGrid grid.heightGrid V.empty, IM.empty)
  | otherwise                                   = formatResult grid =<< buildState grid

buildState ∷ Eq a ⇒ Grid a → ST s (FillState s)
buildState grid = scanAndTuple grid =<< UMV.replicate (totalSize grid) (-1)

scanAndTuple ∷ Eq a ⇒ Grid a → UMV.MVector s Int → ST s (FillState s)
scanAndTuple grid refs = (refs,) <$> scanGrid grid refs (0, 0) 0 IM.empty

scanGrid ∷ Eq a ⇒ Grid a → UMV.MVector s Int → Coordinates → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)
scanGrid grid refs = fix $ \loop coord blockId accMap →
  checkScanGridLoop grid refs loop coord blockId accMap (snd coord >= grid.heightGrid) (fst coord >= grid.widthGrid)

checkScanGridLoop ∷ Eq a ⇒ Grid a → UMV.MVector s Int → LoopFill s → Coordinates → Int → IntMap BlockCoordinates → Bool → Bool → ST s (IntMap BlockCoordinates)
checkScanGridLoop _ _ _ _ _ accMap True _                 = pure accMap
checkScanGridLoop _ _ loop (_, y) blockId accMap _ True   = loop (0, y + 1) blockId accMap
checkScanGridLoop grid refs loop coord blockId accMap _ _ = checkCell grid refs loop coord blockId accMap =<< UMV.unsafeRead refs (toIndexFromGrid coord grid)

checkCell ∷ Eq a ⇒ Grid a → UMV.MVector s Int → LoopFill s → Coordinates → Int → IntMap BlockCoordinates → Int → ST s (IntMap BlockCoordinates)
checkCell grid refs loop coord blockId accMap (-1) = runFill grid refs loop coord blockId accMap (atGrid coord grid)
checkCell _ _ loop (x, y) blockId accMap _         = loop (x + 1, y) blockId accMap

runFill ∷ Eq a ⇒ Grid a → UMV.MVector s Int → LoopFill s → Coordinates → Int → IntMap BlockCoordinates → a → ST s (IntMap BlockCoordinates)
runFill grid refs loop coord@(x, y) blockId accMap targetCol =
  scanNextCol =<< processBlock grid refs targetCol blockId [coord] []
  where scanNextCol coords = loop (x + 1, y) (blockId + 1) (IM.insert blockId coords accMap)

processBlock ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → BlockCoordinates → BlockCoordinates → ST s BlockCoordinates
processBlock grid refs targetCol blockId = fix $ \loop stack acc →
  checkProcessStack grid refs targetCol blockId loop stack acc

checkProcessStack ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → LoopStack s → BlockCoordinates → BlockCoordinates → ST s BlockCoordinates
checkProcessStack _ _ _ _ _ [] acc = pure acc
checkProcessStack grid refs targetCol blockId loop (p : stack) acc =
  checkAndMark grid refs targetCol blockId loop p stack acc =<< UMV.unsafeRead refs (toIndexFromGrid p grid)

checkAndMark ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → LoopStack s → Coordinates → BlockCoordinates → BlockCoordinates → Int → ST s BlockCoordinates
checkAndMark grid refs targetCol blockId loop p@(x, y) stack acc (-1) =
  UMV.unsafeWrite refs (toIndexFromGrid p grid) blockId *> loop (push4Neighbors grid targetCol (x, y) stack) (p : acc)
checkAndMark _ _ _ _ loop _ stack acc _ = loop stack acc

push4Neighbors ∷ Eq a ⇒ Grid a → a → Coordinates → BlockCoordinates → BlockCoordinates
push4Neighbors grid targetCol (x, y) stack =
  pushIfTarget grid targetCol (x + 1, y) $
  pushIfTarget grid targetCol (x - 1, y) $
  pushIfTarget grid targetCol (x, y + 1) $
  pushIfTarget grid targetCol (x, y - 1) stack

pushIfTarget ∷ Eq a ⇒ Grid a → a → Coordinates → BlockCoordinates → BlockCoordinates
pushIfTarget grid targetCol p stack
  | inRangeGrid p grid && unsafeIndex p grid == targetCol = p : stack
  | otherwise                                             = stack
{-# INLINE pushIfTarget #-}

formatResult ∷ Grid a → FillState s → ST s FillResult
formatResult grid (refs, blockMap) = formatGrid blockMap <$> UV.freeze refs
  where formatGrid bMap frozen = (Grid grid.widthGrid grid.heightGrid (V.convert $ UV.map normalizeCell frozen), bMap)

normalizeCell ∷ Int → Int
normalizeCell (-1) = 0
normalizeCell val  = val
