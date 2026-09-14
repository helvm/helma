{-# LANGUAGE BangPatterns #-}
module HelVM.HelMA.Automata.Piet.Types.Grid
  ( Grid (..)
  , atGrid
  , discoverBlock
  , inRangeGrid
  , indexMaybe
  , newGrid
  , nextCoords
  , (&!)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates ( BlockCoordinates, Coordinates )
import qualified HelVM.HelMA.Automata.Piet.Types.Coordinates as Coordinates

import           Control.Monad.ST                            ( ST, runST )

import qualified Data.Vector                                 as V
import qualified Data.Vector.Mutable                         as MV
import qualified Data.Vector.Unboxed.Mutable                 as UMV

-- Grid DEFINITION

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
  | inRangeGrid coord m = Just $ m `unsafeIndex` coord
  | otherwise           = Nothing
{-# INLINE indexMaybe #-}

newGrid ∷ Coordinates → [(Coordinates, a)] → Grid a
newGrid (w, h) elems = Grid w h $ V.create $ MV.unsafeNew (w * h) >>= writeElems elems w

writeElems ∷ [(Coordinates, a)] → Int → MV.MVector s a → ST s (MV.MVector s a)
writeElems elems w vec = traverse_ (uncurry $ MV.write vec . toIndex w) elems $> vec

inRangeGrid ∷ Coordinates → Grid a → Bool
inRangeGrid (x, y) m = x >= 0 && x < widthGrid m && y >= 0 && y < heightGrid m
{-# INLINE inRangeGrid #-}

atGrid ∷ Coordinates → Grid a → a
atGrid coord m
  | inRangeGrid coord m = m `unsafeIndex` coord
  | otherwise           = error $ "Grid.atGrid: Out of bounds " <> show coord
{-# INLINE atGrid #-}

-- UTILS (PRIVATE / INLINE)

unsafeIndex ∷ Grid a → Coordinates → a
unsafeIndex m coord = cells m `V.unsafeIndex` toIndexFromGrid m coord
{-# INLINE unsafeIndex #-}

toIndexFromGrid ∷ Grid a → Coordinates → Int
toIndexFromGrid m = toIndex (widthGrid m)
{-# INLINE toIndexFromGrid #-}

toIndex ∷ Int → Coordinates → Int
toIndex w (x, y) = y * w + x
{-# INLINE toIndex #-}

-- GENERIC FAST BLOCK DISCOVERY (ZERO ALLOCATION BFS)

discoverBlock ∷ Eq a ⇒ Grid a → Coordinates → BlockCoordinates
discoverBlock m startPos
  | not (inRangeGrid startPos m) = []
  | otherwise                    = runST $ do
      let w = widthGrid m
          h = heightGrid m
          totalSize = w * h
      
      visited <- UMV.replicate totalSize False
      
      queueX <- UMV.unsafeNew totalSize
      queueY <- UMV.unsafeNew totalSize
      
      let (startX, startY) = startPos
          targetColor = m `unsafeIndex` startPos
          startIdx = toIndex w startPos
      
      UMV.unsafeWrite visited startIdx True
      UMV.unsafeWrite queueX 0 startX
      UMV.unsafeWrite queueY 0 startY
      
      let loop !headIdx !tailIdx !acc =
            if headIdx >= tailIdx
              then pure acc
              else do
                x <- UMV.unsafeRead queueX headIdx
                y <- UMV.unsafeRead queueY headIdx
                
                let currCoord = (x, y)
                    newAcc = currCoord : acc
                
                -- Bezpośrednie sprawdzenie 4 sąsiadów bez tworzenia tymczasowych list!
                tailIdx' <- pushNeighbour m targetColor visited queueX queueY w h (x + 1) y tailIdx
                tailIdx'' <- pushNeighbour m targetColor visited queueX queueY w h (x - 1) y tailIdx'
                tailIdx''' <- pushNeighbour m targetColor visited queueX queueY w h x (y + 1) tailIdx''
                finalTail <- pushNeighbour m targetColor visited queueX queueY w h x (y - 1) tailIdx'''
                
                loop (headIdx + 1) finalTail newAcc
                
      loop 0 1 []
{-# INLINE discoverBlock #-}

pushNeighbour ∷ Eq a 
              ⇒ Grid a → a → UMV.MVector s Bool 
              → UMV.MVector s Int → UMV.MVector s Int 
              → Int → Int → Int → Int → Int → ST s Int
pushNeighbour m targetColor visited qX qY w h x y tailIdx
  | x >= 0 && x < w && y >= 0 && y < h = do
      let idx = y * w + x
      isVisited <- UMV.unsafeRead visited idx
      if not isVisited && (cells m `V.unsafeIndex` idx == targetColor)
        then do
          UMV.unsafeWrite visited idx True
          UMV.unsafeWrite qX tailIdx x
          UMV.unsafeWrite qY tailIdx y
          pure $! tailIdx + 1
        else pure tailIdx
  | otherwise = pure tailIdx
{-# INLINE pushNeighbour #-}

nextCoords ∷ Grid a → Coordinates → Maybe Coordinates
nextCoords m = Coordinates.nextCoords (widthGrid m, heightGrid m)
