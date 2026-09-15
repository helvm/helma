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
      , cells      :: !(V.Vector a)
      }
  deriving stock (Eq, Show)

instance Functor Grid where
  fmap f (Grid w h pxs) = Grid w h (fmap f pxs)
  {-# INLINE fmap #-}

-- EXPORTED FUNCTIONS & OPERATORS

gridBounds ∷ Grid a → Coordinates
gridBounds a = (widthGrid a, heightGrid a)
{-# INLINE gridBounds #-}

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
newGrid (w, h) elems = Grid w h $ V.create $ do
  vec <- MV.unsafeNew (w * h)
  writeElems elems w vec
  pure vec

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
{-# INLINE nextCoords #-}

-- PRIVATE HELPERS

writeElems ∷ [(Coordinates, a)] → Int → MV.MVector s a → ST s ()
writeElems elems w vec = traverse_ (\(coord, a) -> MV.unsafeWrite vec (toIndex w coord) a) elems
{-# INLINE writeElems #-}

-- BFS OPTIMIZED: Płaska pętla i Unsafe operacje bez alokacji krotek wewnątrz queue
initBfs ∷ Eq a ⇒ Grid a → Coordinates → ST s BlockCoordinates
initBfs m startPos = do
  let sz = totalSize m
  visited <- UMV.replicate sz False
  qX      <- UMV.unsafeNew sz
  qY      <- UMV.unsafeNew sz
  
  let (startX, startY) = startPos
  UMV.unsafeWrite visited (toIndexFromGrid m startPos) True
  UMV.unsafeWrite qX 0 startX
  UMV.unsafeWrite qY 0 startY
  
  loopBfs m (unsafeIndex m startPos) visited qX qY 0 1 []

loopBfs ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → BlockCoordinates → ST s BlockCoordinates
loopBfs m targetCol visited qX qY headIdx tailIdx acc
  | headIdx >= tailIdx = pure acc
  | otherwise = do
      x <- UMV.unsafeRead qX headIdx
      y <- UMV.unsafeRead qY headIdx
      
      -- Wstawianie 4 sąsiadów bezpośrednio inline bez tworzenia monadycznych łańcuchów
      let newHead = headIdx + 1
          acc'    = (x, y) : acc
      
      t1 <- pushNeighbour m targetCol visited qX qY (x + 1) y tailIdx
      t2 <- pushNeighbour m targetCol visited qX qY (x - 1) y t1
      t3 <- pushNeighbour m targetCol visited qX qY x (y + 1) t2
      t4 <- pushNeighbour m targetCol visited qX qY x (y - 1) t3
      
      loopBfs m targetCol visited qX qY newHead t4 acc'

pushNeighbour ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → UMV.MVector s Int → UMV.MVector s Int → Int → Int → Int → ST s Int
pushNeighbour m targetCol visited qX qY x y tailIdx
  | inRangeGrid (x, y) m = do
      let idx = toIndexFromGrid m (x, y)
      isVis <- UMV.unsafeRead visited idx
      if not isVis && unsafeIndex m (x, y) == targetCol
        then do
          UMV.unsafeWrite visited idx True
          UMV.unsafeWrite qX tailIdx x
          UMV.unsafeWrite qY tailIdx y
          pure (tailIdx + 1)
        else pure tailIdx
  | otherwise = pure tailIdx
{-# INLINE pushNeighbour #-}

unsafeIndex ∷ Grid a → Coordinates → a
unsafeIndex m coord = V.unsafeIndex (cells m) (toIndexFromGrid m coord)
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

gridToMatrix ∷ Grid a → Matrix a
gridToMatrix grid = V.generate (heightGrid grid) (extractRow grid)

extractRow ∷ Grid a → Int → Vector a
extractRow grid y = V.slice (y * w) w (cells grid) where
  w = widthGrid grid
