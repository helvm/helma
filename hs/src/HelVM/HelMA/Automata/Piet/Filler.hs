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

-- PUBLIC API

fillAll ∷ Eq a ⇒ Grid a → FillResult
fillAll grid = runST $ fillST grid

-- PRIVATE HELPERS (TOP-DOWN)

fillST ∷ Eq a ⇒ Grid a → ST s FillResult
fillST grid
  | widthGrid grid == 0 || heightGrid grid == 0 = pure (Grid (widthGrid grid) (heightGrid grid) V.empty, IM.empty)
  | otherwise                                   = formatResult grid =<< buildState grid

buildState ∷ Eq a ⇒ Grid a → ST s (FillState s)
buildState grid = UMV.replicate (totalSize grid) (-1) >>= \refs -> (refs,) <$> scanGrid grid refs 0 0 0 IM.empty

scanGrid ∷ Eq a ⇒ Grid a → UMV.MVector s Int → Int → Int → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)
scanGrid grid refs = fix $ \loop y x blockId accMap ->
  checkScanGridLoop grid refs loop y x blockId accMap (y >= heightGrid grid) (x >= widthGrid grid)

checkScanGridLoop ∷ Eq a ⇒ Grid a → UMV.MVector s Int → (Int → Int → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)) → Int → Int → Int → IntMap BlockCoordinates → Bool → Bool → ST s (IntMap BlockCoordinates)
checkScanGridLoop _ _ _ _ _ _ accMap True _             = pure accMap
checkScanGridLoop _ _ loop y _ blockId accMap _ True    = loop (y + 1) 0 blockId accMap
checkScanGridLoop grid refs loop y x blockId accMap _ _ = checkCell grid refs loop y x blockId accMap =<< UMV.unsafeRead refs (toIndexFromGrid grid (x, y))

checkCell ∷ Eq a ⇒ Grid a → UMV.MVector s Int → (Int → Int → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)) → Int → Int → Int → IntMap BlockCoordinates → Int → ST s (IntMap BlockCoordinates)
checkCell grid refs loop y x blockId accMap (-1) = runFill grid refs loop y x blockId accMap (atGrid (x, y) grid)
checkCell _ _ loop y x blockId accMap _          = loop y (x + 1) blockId accMap

runFill ∷ Eq a ⇒ Grid a → UMV.MVector s Int → (Int → Int → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)) → Int → Int → Int → IntMap BlockCoordinates → a → ST s (IntMap BlockCoordinates)
runFill grid refs loop y x blockId accMap targetCol =
  scanNextCol =<< processBlock grid refs targetCol blockId [(x, y)] [] where
    scanNextCol coords = loop y (x + 1) (blockId + 1) (IM.insert blockId coords accMap)

processBlock ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → BlockCoordinates → BlockCoordinates → ST s BlockCoordinates
processBlock grid refs targetCol blockId = fix $ \loop stack acc ->
  checkProcessStack grid refs targetCol blockId loop stack acc

checkProcessStack ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → (BlockCoordinates → BlockCoordinates → ST s BlockCoordinates) → BlockCoordinates → BlockCoordinates → ST s BlockCoordinates
checkProcessStack _ _ _ _ _ [] acc = pure acc
checkProcessStack grid refs targetCol blockId loop (p : stack) acc =
  checkAndMark grid refs targetCol blockId loop p stack acc =<< UMV.unsafeRead refs (toIndexFromGrid grid p)

checkAndMark ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → (BlockCoordinates → BlockCoordinates → ST s BlockCoordinates) → Coordinates → BlockCoordinates → BlockCoordinates → Int → ST s BlockCoordinates
checkAndMark grid refs targetCol blockId loop p stack acc (-1) =
  UMV.unsafeWrite refs (toIndexFromGrid grid p) blockId *>
    loop (validNeighbors grid p targetCol ++ stack) (p : acc)
checkAndMark _ _ _ _ loop _ stack acc _ = loop stack acc

validNeighbors ∷ Eq a ⇒ Grid a → Coordinates → a → BlockCoordinates
validNeighbors grid (x, y) targetCol = filter (isTarget grid targetCol) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isTarget ∷ Eq a ⇒ Grid a → a → Coordinates → Bool
isTarget grid targetCol p = inRangeGrid p grid && atGrid p grid == targetCol

formatResult ∷ Grid a → FillState s → ST s FillResult
formatResult grid (refs, blockMap) = formatGrid blockMap <$> UV.freeze refs where
  formatGrid bMap frozen = (Grid (widthGrid grid) (heightGrid grid) (V.convert $ UV.map normalizeCell frozen), bMap)

normalizeCell ∷ Int → Int
normalizeCell (-1) = 0
normalizeCell val  = val
