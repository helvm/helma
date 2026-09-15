module HelVM.HelMA.Automata.Piet.Filler
  ( fillAll
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Grid

import           Control.Monad.ST                            ( ST, runST )

import qualified Data.IntMap.Strict                          as IM
import qualified Data.Vector                                 as V
import qualified Data.Vector.Unboxed.Mutable                 as UMV

-- PUBLIC API

fillAll ∷ Eq a ⇒ Grid a → (Matrix Int, IntMap BlockCoordinates)
fillAll grid = runST $ fillST image $ matrixBounds image where image = gridToMatrix grid

-- PRIVATE HELPERS (TOP-DOWN)

fillST ∷ Eq a ⇒ Matrix a → Coordinates → ST s (Matrix Int, IntMap BlockCoordinates)
fillST _     (0, _) = pure (V.empty, IM.empty)
fillST image (_, 0) = pure (V.map (const V.empty) image, IM.empty)
fillST image dim    = formatResult image dim =<< buildState image dim

buildState ∷ Eq a ⇒ Matrix a → Coordinates → ST s (UMV.MVector s Int, IntMap BlockCoordinates)
buildState image dim@(h, w) = UMV.replicate (h * w) (-1) >>= \refs -> (refs,) <$> scanGrid image dim refs 0 0 0 IM.empty

scanGrid ∷ Eq a ⇒ Matrix a → Coordinates → UMV.MVector s Int → Int → Int → Int → IntMap BlockCoordinates → ST s (IntMap BlockCoordinates)
scanGrid _ (h, _) _ y _ _ accMap | y >= h = pure accMap
scanGrid image dim@(_, w) refs y x blockId accMap
  | x >= rowLen y = scanGrid image dim refs (y + 1) 0 blockId accMap
  | otherwise     = checkCell image dim refs y x blockId accMap =<< UMV.unsafeRead refs (y * w + x)
  where
    rowLen = V.length . (image V.!)

checkCell ∷ Eq a ⇒ Matrix a → Coordinates → UMV.MVector s Int → Int → Int → Int → IntMap BlockCoordinates → Int → ST s (IntMap BlockCoordinates)
checkCell image dim refs y x blockId accMap (-1) = maybe (scanGrid image dim refs y (x + 1) blockId accMap) (runFill image dim refs y x blockId accMap) (getPixel image x y)
checkCell image dim refs y x blockId accMap _    = scanGrid image dim refs y (x + 1) blockId accMap

runFill ∷ Eq a ⇒ Matrix a → Coordinates → UMV.MVector s Int → Int → Int → Int → IntMap BlockCoordinates → a → ST s (IntMap BlockCoordinates)
runFill image dim refs y x blockId accMap targetCol =
  processBlock image dim refs targetCol blockId [(x, y)] [] >>= \coords ->
    scanGrid image dim refs y (x + 1) (blockId + 1) (IM.insert blockId coords accMap)

processBlock ∷ Eq a ⇒ Matrix a → Coordinates → UMV.MVector s Int → a → Int → BlockCoordinates → BlockCoordinates → ST s BlockCoordinates
processBlock _ _ _ _ _ [] acc = pure acc
processBlock image dim@(_, w) refs targetCol blockId (p : stack) acc =
  checkAndMark image dim refs targetCol blockId p stack acc =<< UMV.unsafeRead refs (idx p w)

checkAndMark ∷ Eq a ⇒ Matrix a → Coordinates → UMV.MVector s Int → a → Int → Coordinates → BlockCoordinates → BlockCoordinates → Int → ST s BlockCoordinates
checkAndMark image dim@(h, w) refs targetCol blockId p stack acc (-1) =
  UMV.unsafeWrite refs (idx p w) blockId *>
    processBlock image dim refs targetCol blockId (validNeighbors image h p targetCol ++ stack) (p : acc)
checkAndMark image dim refs targetCol blockId _ stack acc _ =
  processBlock image dim refs targetCol blockId stack acc

validNeighbors ∷ Eq a ⇒ Matrix a → Int → Coordinates → a → BlockCoordinates
validNeighbors image h (x, y) targetCol = filter (isTarget image h targetCol) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isTarget ∷ Eq a ⇒ Matrix a → Int → a → Coordinates → Bool
isTarget image h targetCol (nx, ny) = ny >= 0 && ny < h && nx >= 0 && nx < rowLen ny && getPixel image nx ny == Just targetCol where
  rowLen y = V.length (image V.! y)

formatResult ∷ Matrix a → Coordinates → (UMV.MVector s Int, IntMap BlockCoordinates) → ST s (Matrix Int, IntMap BlockCoordinates)
formatResult image p (refs, blockMap) = (, blockMap) <$> formatResult' image p refs

formatResult' ∷ UMV.PrimMonad m ⇒ Matrix a → Coordinates → UMV.MVector (UMV.PrimState m) Int → m (Matrix Int)
formatResult' image (h, w) refs = V.generateM h (\y -> V.generateM (V.length (image V.! y)) (\x -> normalizeCell <$> UMV.unsafeRead refs (y * w + x)))

normalizeCell ∷ Int → Int
normalizeCell (-1) = 0
normalizeCell val  = val

matrixBounds ∷ Matrix a → Coordinates
matrixBounds img = (V.length img, V.foldl' (\acc r -> max acc (V.length r)) 0 img)

getPixel ∷ Matrix a → Int → Int → Maybe a
getPixel img x y = (V.!? x) =<< (img V.!? y)

idx ∷ Coordinates → Int → Int
idx (x, y) w = y * w + x
