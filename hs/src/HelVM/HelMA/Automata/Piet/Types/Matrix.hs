module HelVM.HelMA.Automata.Piet.Types.Matrix
  ( Matrix (..)
  , discoverBlock
  , inRangeMatrix
  , newMatrix
  , pixelMatrix
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
m &! coord
  | inRangeMatrix coord m = Just $ m `unsafeIndex` coord
  | otherwise             = Nothing
{-# INLINE (&!) #-}

newMatrix ∷ Coordinates → [(Coordinates, a)] → Matrix a
newMatrix (w, h) elems = Matrix w h $ V.create $ MV.new (w * h) >>= \vec ->
  traverse_ (uncurry $ MV.write vec . toIndex w) elems >> pure vec

inRangeMatrix ∷ Coordinates → Matrix a → Bool
inRangeMatrix (x, y) m = x >= 0 && x < widthMatrix m && y >= 0 && y < heightMatrix m
{-# INLINE inRangeMatrix #-}

pixelMatrix ∷ Coordinates → Matrix a → a
pixelMatrix coord m
  | inRangeMatrix coord m = m `unsafeIndex` coord
  | otherwise             = error $ "Matrix.pixelMatrix: Out of bounds " <> show coord
{-# INLINE pixelMatrix #-}

-- UTILS (PRIVATE / INLINE)

unsafeIndex ∷ Matrix a → Coordinates → a
unsafeIndex  m coord = pixels m `V.unsafeIndex` toIndex (widthMatrix m) coord
{-# INLINE unsafeIndex #-}

toIndex ∷ Int → Coordinates → Int
toIndex w (x, y) = y * w + x
{-# INLINE toIndex #-}

-- GENERIC FAST BLOCK DISCOVERY

discoverBlock ∷ Eq a ⇒ Matrix a → Coordinates → Block
discoverBlock m startPos
  | not (inRangeMatrix startPos m) = []
  | otherwise                      = runST $ UMV.replicate (widthMatrix m * heightMatrix m) False >>= runBfs m startPos

runBfs ∷ Eq a ⇒ Matrix a → Coordinates → UMV.MVector s Bool → ST s Block
runBfs m startPos visited = bfs m (m `unsafeIndex` startPos) visited [startPos] []

bfs ∷ Eq a ⇒ Matrix a → a → UMV.MVector s Bool → [Coordinates] → Block → ST s Block
bfs _ _ _ [] acc = pure acc
bfs m targetColor visited (curr : rest) acc =
  UMV.unsafeRead visited idx >>= processCell m targetColor visited curr rest acc idx
  where
    idx = toIndex (widthMatrix m) curr

processCell ∷ Eq a ⇒ Matrix a → a → UMV.MVector s Bool → Coordinates → [Coordinates] → Block → Int → Bool → ST s Block
processCell m targetColor visited curr rest acc idx isVisited
  | isVisited                               = bfs m targetColor visited rest acc
  | m `unsafeIndex` curr /= targetColor = UMV.unsafeWrite visited idx True >> bfs m targetColor visited rest acc
  | otherwise                               = UMV.unsafeWrite visited idx True >> bfs m targetColor visited (filter (`inRangeMatrix` m) (neighbours curr) ++ rest) (curr : acc)
