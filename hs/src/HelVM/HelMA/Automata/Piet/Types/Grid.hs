module HelVM.HelMA.Automata.Piet.Types.Grid
  ( Grid (..)
  , Matrix
  , STMatrix
  , atGrid
  , discoverBlock
  , inRangeGrid
  , indexMaybe
  , matrixToGrid
  , newGrid
  , nextCoords
  , (&!)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates ( BlockCoordinates, Coordinates )
import qualified HelVM.HelMA.Automata.Piet.Types.Coordinates as Coordinates

import           Control.Monad.ST                            ( ST, runST )

import           Data.Vector                                 ( Vector )
import qualified Data.Vector                                 as V
import           Data.Vector.Mutable                         ( STVector )
import qualified Data.Vector.Mutable                         as MV
import qualified Data.Vector.Unboxed.Mutable                 as UMV

-- Grid DEFINITION

type Matrix a = Vector (Vector a)
type STMatrix s b = Vector (STVector s (Maybe b))

data Grid a
  = Grid
      { widthGrid  :: {-# UNPACK #-} !Int
      , heightGrid :: {-# UNPACK #-} !Int
      , cells      :: !(V.Vector a)
      }
  deriving stock (Eq, Show)

instance Functor Grid where
  fmap f (Grid w h pxs) = Grid w h (fmap f pxs)

-- EXPORTED FUNCTIONS & OPERATORS

infixl 9 &!
(&!) ∷ Grid a → Coordinates → Maybe a
(&!) = indexMaybe
{-# INLINE (&!) #-}

indexMaybe ∷ Grid a → Coordinates → Maybe a
indexMaybe m coord
  | inRangeGrid coord m = Just $ unsafeIndex m coord
  | otherwise           = Nothing
{-# INLINE indexMaybe #-}

newGrid ∷ Coordinates → [(Coordinates, a)] → Grid a
newGrid (w, h) elems = Grid w h $ V.create $ writeElems elems w =<< MV.unsafeNew (w * h)

inRangeGrid ∷ Coordinates → Grid a → Bool
inRangeGrid (x, y) m = x >= 0 && x < widthGrid m && y >= 0 && y < heightGrid m
{-# INLINE inRangeGrid #-}

atGrid ∷ Coordinates → Grid a → a
atGrid coord m
  | inRangeGrid coord m = unsafeIndex m coord
  | otherwise           = error $ "Grid.atGrid: Out of bounds " <> show coord
{-# INLINE atGrid #-}

discoverBlock ∷ Eq a ⇒ Grid a → Coordinates → BlockCoordinates
discoverBlock m startPos
  | inRangeGrid startPos m = runST $ initBfs m startPos
  | otherwise              = []
{-# INLINE discoverBlock #-}

nextCoords ∷ Grid a → Coordinates → Maybe Coordinates
nextCoords m = Coordinates.nextCoords (widthGrid m, heightGrid m)

-- PRIVATE HELPERS (TOP-DOWN)

writeElems ∷ [(Coordinates, a)] → Int → MV.MVector s a → ST s (MV.MVector s a)
writeElems elems w vec = traverse_ (uncurry $ MV.write vec . toIndex w) elems $> vec

initBfs ∷ Eq a ⇒ Grid a → Coordinates → ST s BlockCoordinates
initBfs m startPos =
  UMV.replicate (totalSize m) False >>= \visited ->
    UMV.unsafeNew (totalSize m) >>= \qX ->
      UMV.unsafeNew (totalSize m) >>= \qY ->
        setupAndLoop m startPos visited qX qY

setupAndLoop ∷ Eq a ⇒ Grid a → Coordinates → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → ST s BlockCoordinates
setupAndLoop m (startX, startY) visited qX qY =
  UMV.unsafeWrite visited (toIndexFromGrid m (startX, startY)) True *>
    UMV.unsafeWrite qX 0 startX *>
      UMV.unsafeWrite qY 0 startY *>
        loopBfs m (unsafeIndex m (startX, startY)) visited qX qY 0 1 []

loopBfs ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → BlockCoordinates → ST s BlockCoordinates
loopBfs _ _ _ _ _ headIdx tailIdx acc | headIdx >= tailIdx = pure acc
loopBfs m targetCol visited qX qY headIdx tailIdx acc =
  UMV.unsafeRead qX headIdx >>= \x ->
    UMV.unsafeRead qY headIdx >>= \y ->
      pushNeighbours m targetCol visited qX qY x y tailIdx >>= \newTail ->
        loopBfs m targetCol visited qX qY (headIdx + 1) newTail ((x, y) : acc)

pushNeighbours ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → Int → ST s Int
pushNeighbours m targetCol visited qX qY x y tailIdx =
  pushNeighbour m targetCol visited qX qY (x + 1) y tailIdx >>=
    pushNeighbour m targetCol visited qX qY (x - 1) y >>=
      pushNeighbour m targetCol visited qX qY x (y + 1) >>=
        pushNeighbour m targetCol visited qX qY x (y - 1)

pushNeighbour ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → Int → ST s Int
pushNeighbour m targetCol visited qX qY x y tailIdx
  | inRangeGrid (x, y) m = checkUnvisited m targetCol visited qX qY x y tailIdx =<< UMV.unsafeRead visited (toIndexFromGrid m (x, y))
  | otherwise            = pure tailIdx

checkUnvisited ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → Int → Bool → ST s Int
checkUnvisited _ _ _ _ _ _ _ tailIdx True = pure tailIdx
checkUnvisited m targetCol visited qX qY x y tailIdx False
  | unsafeIndex m (x, y) == targetCol = enqueueNeighbor visited qX qY x y tailIdx (toIndexFromGrid m (x, y))
  | otherwise                         = pure tailIdx

enqueueNeighbor ∷ UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → Int → Int → ST s Int
enqueueNeighbor visited qX qY x y tailIdx idxVal =
  UMV.unsafeWrite visited idxVal True *>
    UMV.unsafeWrite qX tailIdx x *>
      UMV.unsafeWrite qY tailIdx y $>
        (tailIdx + 1)

unsafeIndex ∷ Grid a → Coordinates → a
unsafeIndex m coord = cells m `V.unsafeIndex` toIndexFromGrid m coord
{-# INLINE unsafeIndex #-}

toIndexFromGrid ∷ Grid a → Coordinates → Int
toIndexFromGrid m = toIndex (widthGrid m)
{-# INLINE toIndexFromGrid #-}

toIndex ∷ Int → Coordinates → Int
toIndex w (x, y) = y * w + x
{-# INLINE toIndex #-}

totalSize ∷ Grid a → Int
totalSize m = widthGrid m * heightGrid m
{-# INLINE totalSize #-}

matrixToGrid ∷ Matrix a → Grid a
matrixToGrid matrix = Grid w h (V.concat $ V.toList matrix) where
  h = V.length matrix
  w = maybe 0 V.length (matrix V.!? 0)
