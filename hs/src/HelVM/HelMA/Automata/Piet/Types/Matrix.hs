module HelVM.HelMA.Automata.Piet.Types.Matrix
  ( Matrix (..)
  , atMatrix
  , discoverBlock
  , inRangeMatrix
  , newMatrix
  , (&!)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Control.Monad.ST                            ( ST, runST )

import qualified Data.Vector                                 as V
import qualified Data.Vector.Mutable                         as MV
import qualified Data.Vector.Unboxed.Mutable                 as UMV

-- MATRIX DEFINITION

data Matrix a
  = Matrix
      { widthMatrix  :: {-# UNPACK #-} !Int
      , heightMatrix :: {-# UNPACK #-} !Int
      , pixels       :: !(V.Vector a)
      }
  deriving stock (Eq, Show)

instance Functor Matrix where
  fmap f (Matrix w h pxs) = Matrix w h (fmap f pxs)

-- EXPORTED FUNCTIONS & OPERATORS

infixl 9 &!
(&!) ∷ Matrix a → Coordinates → Maybe a
(&!) = indexMaybe
{-# INLINE (&!) #-}

indexMaybe ∷ Matrix a → Coordinates → Maybe a
indexMaybe m coord
  | inRangeMatrix coord m = Just $ m `indexUncheck` coord
  | otherwise             = Nothing
{-# INLINE indexMaybe #-}

newMatrix ∷ Coordinates → [(Coordinates, a)] → Matrix a
newMatrix (w, h) elems = Matrix w h $ V.create $ MV.unsafeNew (w * h) >>= writeElems elems w

writeElems ∷ [(Coordinates, a)] → Int → MV.MVector s a → ST s (MV.MVector s a)
writeElems elems w vec = traverse_ (uncurry $ MV.write vec . toIndex w) elems >> pure vec

inRangeMatrix ∷ Coordinates → Matrix a → Bool
inRangeMatrix (x, y) m = x >= 0 && x < widthMatrix m && y >= 0 && y < heightMatrix m
{-# INLINE inRangeMatrix #-}

atMatrix ∷ Coordinates → Matrix a → a
atMatrix coord m
  | inRangeMatrix coord m = m `indexUncheck` coord
  | otherwise             = error $ "Matrix.atMatrix: Out of bounds " <> show coord
{-# INLINE atMatrix #-}

-- UTILS (PRIVATE / INLINE)

indexUncheck ∷ Matrix a → Coordinates → a
indexUncheck  m coord = pixels m `V.unsafeIndex` toIndexFromMatrix m coord
{-# INLINE indexUncheck #-}

toIndexFromMatrix ∷ Matrix a → Coordinates → Int
toIndexFromMatrix m = toIndex (widthMatrix m)
{-# INLINE toIndexFromMatrix #-}

toIndex ∷ Int → Coordinates → Int
toIndex w (x, y) = y * w + x
{-# INLINE toIndex #-}

-- GENERIC FAST BLOCK DISCOVERY

discoverBlock ∷ Eq a ⇒ Matrix a → Coordinates → Block
discoverBlock m startPos
  | not (inRangeMatrix startPos m) = []
  | otherwise                      = runST $ UMV.replicate (widthMatrix m * heightMatrix m) False >>= runBfs m startPos

runBfs ∷ Eq a ⇒ Matrix a → Coordinates → UMV.MVector s Bool → ST s Block
runBfs m startPos visited = bfs m (m `indexUncheck` startPos) visited [startPos] []

bfs ∷ Eq a ⇒ Matrix a → a → UMV.MVector s Bool → [Coordinates] → Block → ST s Block
bfs _ _ _ [] acc = pure acc
bfs m targetColor visited (curr : rest) acc = UMV.unsafeRead visited idx >>= processCell m targetColor visited curr rest acc idx where
  idx = toIndexFromMatrix m curr

processCell ∷ Eq a ⇒ Matrix a → a → UMV.MVector s Bool → Coordinates → [Coordinates] → Block → Int → Bool → ST s Block
processCell m targetColor visited _    rest acc _   True  = bfs m targetColor visited rest acc
processCell m targetColor visited curr rest acc idx False = UMV.unsafeWrite visited idx True >> checkColor m targetColor visited curr rest acc (m `indexUncheck` curr == targetColor)

checkColor ∷ Eq a ⇒ Matrix a → a → UMV.MVector s Bool → Coordinates → [Coordinates] → Block → Bool → ST s Block
checkColor m targetColor visited _ rest acc False = bfs m targetColor visited rest acc
checkColor m targetColor visited curr rest acc True  =
  bfs m targetColor visited (validNeighbours m curr ++ rest) (curr : acc)

validNeighbours ∷ Matrix a → Coordinates → [Coordinates]
validNeighbours m (x, y) = filter (`inRangeMatrix` m) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
