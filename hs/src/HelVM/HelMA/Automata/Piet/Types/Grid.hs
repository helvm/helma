module HelVM.HelMA.Automata.Piet.Types.Grid
  ( Grid (..)
  , Matrix
  , atGrid
  , discoverBlock
  , gridBounds
  , gridToMatrix
  , inRangeGrid
  , indexMaybe
  , matrixToGrid
  , newGrid
  , nextCoords
  , toIndexFromGrid
  , totalSize
  , (&!)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates ( BlockCoordinates, Coordinates )
import qualified HelVM.HelMA.Automata.Piet.Types.Coordinates as Coordinates

import           Control.Monad.ST                            ( ST, runST )

import           Data.Vector                                 ( Vector )
import qualified Data.Vector                                 as V
import qualified Data.Vector.Mutable                         as MV
import qualified Data.Vector.Unboxed.Mutable                 as UMV

-- Grid DEFINITION

type Matrix a = Vector (Vector a)

data Grid a
  = Grid
      { widthGrid  :: {-# UNPACK #-} !Int
      , heightGrid :: {-# UNPACK #-} !Int
      , cells      :: !(Vector a)
      }
  deriving stock (Eq, Show)

instance Functor Grid where
  fmap f g = g { cells = fmap f g.cells }
  {-# INLINE fmap #-}

-- EXPORTED FUNCTIONS & OPERATORS

gridBounds ∷ Grid a → Coordinates
gridBounds g = (g.widthGrid, g.heightGrid)
{-# INLINE gridBounds #-}

infixl 9 &!
(&!) ∷ Grid a → Coordinates → Maybe a
(&!) = indexMaybe
{-# INLINE (&!) #-}

indexMaybe ∷ Grid a → Coordinates → Maybe a
indexMaybe g coord = bool Nothing (Just $ unsafeIndex coord g) $ inRangeGrid coord g
{-# INLINE indexMaybe #-}

newGrid ∷ Coordinates → [(Coordinates, a)] → Grid a
newGrid (w, h) elems = Grid w h $ V.create $ writeElems elems w =<< MV.unsafeNew (w * h)

inRangeGrid ∷ Coordinates → Grid a → Bool
inRangeGrid (x, y) g = x >= 0 && x < g.widthGrid && y >= 0 && y < g.heightGrid
{-# INLINE inRangeGrid #-}

atGrid ∷ Coordinates → Grid a → a
atGrid coord g = bool (error $ "Grid.atGrid: Out of bounds " <> show coord) (unsafeIndex coord g) $ inRangeGrid coord g
{-# INLINE atGrid #-}

discoverBlock ∷ Eq a ⇒ Grid a → Coordinates → BlockCoordinates
discoverBlock g startPos = bool [] (runST $ initBfs startPos g) $ inRangeGrid startPos g
{-# INLINE discoverBlock #-}

nextCoords ∷ Grid a → Coordinates → Maybe Coordinates
nextCoords g = Coordinates.nextCoords (g.widthGrid, g.heightGrid)
{-# INLINE nextCoords #-}

matrixToGrid ∷ Matrix a → Grid a
matrixToGrid m = Grid (maybe 0 V.length $ m V.!? 0) (V.length m) (V.concat $ V.toList m)

gridToMatrix ∷ Grid a → Matrix a
gridToMatrix g = V.generate g.heightGrid $ extractRow g

toIndexFromGrid ∷ Coordinates → Grid a → Int
toIndexFromGrid coord g = toIndex g.widthGrid coord
{-# INLINE toIndexFromGrid #-}

totalSize ∷ Grid a → Int
totalSize g = g.widthGrid * g.heightGrid
{-# INLINE totalSize #-}

-- PRIVATE HELPERS

writeElems ∷ [(Coordinates, a)] → Int → MV.MVector s a → ST s (MV.MVector s a)
writeElems elems w vec = vec <$ traverse_ (passElem w vec) elems
{-# INLINE writeElems #-}

passElem ∷ Int → MV.MVector s a → (Coordinates, a) → ST s ()
passElem w vec (coord, val) = MV.unsafeWrite vec (toIndex w coord) val
{-# INLINE passElem #-}

initBfs ∷ Eq a ⇒ Coordinates → Grid a → ST s BlockCoordinates
initBfs startPos g = join $ setupAndLoop startPos g <$> UMV.replicate sz False <*> UMV.unsafeNew sz <*> UMV.unsafeNew sz
  where sz = totalSize g

setupAndLoop ∷ Eq a ⇒ Coordinates → Grid a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → ST s BlockCoordinates
setupAndLoop (x, y) g visited qX qY =
  UMV.unsafeWrite visited (toIndexFromGrid (x, y) g) True
    *> UMV.unsafeWrite qX 0 x
    *> UMV.unsafeWrite qY 0 y
    *> loopBfs (unsafeIndex (x, y) g) visited qX qY g 0 1 []

loopBfs ∷ Eq a ⇒ a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Grid a → Int → Int → BlockCoordinates → ST s BlockCoordinates
loopBfs targetCol visited qX qY g = fix $ \self headIdx tailIdx acc →
  bool (stepBfs self targetCol visited qX qY g headIdx tailIdx acc) (pure acc) (headIdx >= tailIdx)

stepBfs ∷ Eq a ⇒ (Int → Int → BlockCoordinates → ST s BlockCoordinates) → a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Grid a → Int → Int → BlockCoordinates → ST s BlockCoordinates
stepBfs self targetCol visited qX qY g headIdx tailIdx acc =
  (,) <$> UMV.unsafeRead qX headIdx <*> UMV.unsafeRead qY headIdx >>= \(x, y) →
    pushNeighbours targetCol visited qX qY g x y tailIdx >>= \newTail →
      self (headIdx + 1) newTail ((x, y) : acc)

pushNeighbours ∷ Eq a ⇒ a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Grid a → Int → Int → Int → ST s Int
pushNeighbours targetCol visited qX qY g x y tailIdx =
  pushNeighbour targetCol visited qX qY g (x + 1) y tailIdx
    >>= pushNeighbour targetCol visited qX qY g (x - 1) y
    >>= pushNeighbour targetCol visited qX qY g x (y + 1)
    >>= pushNeighbour targetCol visited qX qY g x (y - 1)

pushNeighbour ∷ Eq a ⇒ a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Grid a → Int → Int → Int → ST s Int
pushNeighbour targetCol visited qX qY g x y tailIdx =
  bool (pure tailIdx) (checkUnvisited targetCol visited qX qY g x y tailIdx =<< UMV.unsafeRead visited idx) $ inRangeGrid (x, y) g
  where idx = toIndexFromGrid (x, y) g

checkUnvisited ∷ Eq a ⇒ a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Grid a → Int → Int → Int → Bool → ST s Int
checkUnvisited _ _ _ _ _ _ _ tailIdx True = pure tailIdx
checkUnvisited targetCol visited qX qY g x y tailIdx False =
  bool (pure tailIdx) (enqueueNeighbor visited qX qY x y tailIdx $ toIndexFromGrid (x, y) g) $ unsafeIndex (x, y) g == targetCol

enqueueNeighbor ∷ UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → Int → Int → ST s Int
enqueueNeighbor visited qX qY x y tailIdx idxVal =
  (tailIdx + 1) <$ UMV.unsafeWrite visited idxVal True
    <* UMV.unsafeWrite qX tailIdx x
    <* UMV.unsafeWrite qY tailIdx y

extractRow ∷ Grid a → Int → Vector a
extractRow g y = V.slice (y * g.widthGrid) g.widthGrid g.cells

unsafeIndex ∷ Coordinates → Grid a → a
unsafeIndex coord g = V.unsafeIndex g.cells $ toIndexFromGrid coord g
{-# INLINE unsafeIndex #-}

toIndex ∷ Int → Coordinates → Int
toIndex w (x, y) = y * w + x
{-# INLINE toIndex #-}
