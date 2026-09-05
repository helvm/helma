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
  | otherwise             = Nothing
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
  | otherwise             = error $ "Grid.atGrid: Out of bounds " <> show coord
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

-- GENERIC FAST BLOCK DISCOVERY

discoverBlock ∷ Eq a ⇒ Grid a → Coordinates → BlockCoordinates
discoverBlock m startPos
  | not (inRangeGrid startPos m) = []
  | otherwise                      = runST $ UMV.replicate (widthGrid m * heightGrid m) False >>= runBfs m startPos

runBfs ∷ Eq a ⇒ Grid a → Coordinates → UMV.MVector s Bool → ST s BlockCoordinates
runBfs m startPos visited = bfs m (m `unsafeIndex` startPos) visited [startPos] []

bfs ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → BlockCoordinates → BlockCoordinates → ST s BlockCoordinates
bfs _ _ _ [] acc = pure acc
bfs m targetColor visited (curr : rest) acc = UMV.unsafeRead visited idx >>= processCell m targetColor visited curr rest acc idx where
  idx = toIndexFromGrid m curr

processCell ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → Coordinates → BlockCoordinates → BlockCoordinates → Int → Bool → ST s BlockCoordinates
processCell m targetColor visited _    rest acc _   True  = bfs m targetColor visited rest acc
processCell m targetColor visited curr rest acc idx False = UMV.unsafeWrite visited idx True *> checkColor m targetColor visited curr rest acc (m `unsafeIndex` curr == targetColor)

checkColor ∷ Eq a ⇒ Grid a → a → UMV.MVector s Bool → Coordinates → BlockCoordinates → BlockCoordinates → Bool → ST s BlockCoordinates
checkColor m targetColor visited _ rest acc False = bfs m targetColor visited rest acc
checkColor m targetColor visited curr rest acc True  =
  bfs m targetColor visited (validNeighbours m curr ++ rest) (curr : acc)

validNeighbours ∷ Grid a → Coordinates → BlockCoordinates
validNeighbours m (x, y) = filter (`inRangeGrid` m) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

nextCoords ∷ Grid a → Coordinates → Maybe Coordinates
nextCoords m = Coordinates.nextCoords (widthGrid m, heightGrid m)
