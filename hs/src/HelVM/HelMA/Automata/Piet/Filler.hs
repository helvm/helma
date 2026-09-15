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

-- PUBLIC API

fillAll ∷ Eq a ⇒ Grid a → (Grid Int, IntMap BlockCoordinates)
fillAll grid = runST $ fillST grid

-- PRIVATE HELPERS (TOP-DOWN)

fillST ∷ Eq a ⇒ Grid a → ST s (Grid Int, IntMap BlockCoordinates)
fillST grid
  | widthGrid grid == 0 || heightGrid grid == 0 = pure (Grid (widthGrid grid) (heightGrid grid) V.empty, IM.empty)
  | otherwise                                   = formatResult grid =<< buildState grid

buildState ∷ Eq a ⇒ Grid a → ST s (UMV.MVector s Int, IntMap BlockCoordinates)
buildState grid = UMV.replicate (totalSize grid) (-1) >>= \refs -> (refs,) <$> scanGrid grid refs 0 0 0 IM.empty

scanGrid ∷ Eq a ⇒ Grid a → UMV.MVector s Int → Int → Int → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)
scanGrid grid refs y x blockId accMap
  | y >= heightGrid grid = pure accMap
  | x >= widthGrid grid  = scanGrid grid refs (y + 1) 0 blockId accMap
  | otherwise            = checkCell grid refs y x blockId accMap =<< UMV.unsafeRead refs (toIndexFromGrid grid (x, y))

checkCell ∷ Eq a ⇒ Grid a → UMV.MVector s Int → Int → Int → Int → IntMap BlockCoordinates → Int → ST s (IntMap BlockCoordinates)
checkCell grid refs y x blockId accMap (-1) = runFill grid refs y x blockId accMap (atGrid (x, y) grid)
checkCell grid refs y x blockId accMap _    = scanGrid grid refs y (x + 1) blockId accMap

runFill ∷ Eq a ⇒ Grid a → UMV.MVector s Int → Int → Int → Int → IntMap BlockCoordinates → a → ST s (IntMap BlockCoordinates)
runFill grid refs y x blockId accMap targetCol =
  processBlock grid refs targetCol blockId [(x, y)] [] >>= \coords ->
    scanGrid grid refs y (x + 1) (blockId + 1) (IM.insert blockId coords accMap)

processBlock ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → BlockCoordinates → BlockCoordinates → ST s BlockCoordinates
processBlock _ _ _ _ [] acc = pure acc
processBlock grid refs targetCol blockId (p : stack) acc =
  checkAndMark grid refs targetCol blockId p stack acc =<< UMV.unsafeRead refs (toIndexFromGrid grid p)

checkAndMark ∷ Eq a ⇒ Grid a → UMV.MVector s Int → a → Int → Coordinates → BlockCoordinates → BlockCoordinates → Int → ST s BlockCoordinates
checkAndMark grid refs targetCol blockId p stack acc (-1) =
  UMV.unsafeWrite refs (toIndexFromGrid grid p) blockId *>
    processBlock grid refs targetCol blockId (validNeighbors grid p targetCol ++ stack) (p : acc)
checkAndMark grid refs targetCol blockId _ stack acc _ =
  processBlock grid refs targetCol blockId stack acc

validNeighbors ∷ Eq a ⇒ Grid a → Coordinates → a → BlockCoordinates
validNeighbors grid (x, y) targetCol = filter (isTarget grid targetCol) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isTarget ∷ Eq a ⇒ Grid a → a → Coordinates → Bool
isTarget grid targetCol p = inRangeGrid p grid && atGrid p grid == targetCol

formatResult ∷ Grid a → (UMV.MVector s Int, IntMap BlockCoordinates) → ST s (Grid Int, IntMap BlockCoordinates)
formatResult grid (refs, blockMap) = do
  frozenRefs <- UV.freeze refs
  let normalizedCells = V.convert $ UV.map normalizeCell frozenRefs
  pure (Grid (widthGrid grid) (heightGrid grid) normalizedCells, blockMap)

normalizeCell ∷ Int → Int
normalizeCell (-1) = 0
normalizeCell val  = val
