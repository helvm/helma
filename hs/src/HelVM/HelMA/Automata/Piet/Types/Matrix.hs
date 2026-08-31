module HelVM.HelMA.Automata.Piet.Types.Matrix
  ( Matrix (..)
  , discoverBlock
  , heightMatrix
  , inRangeMatrix
  , newMatrix
  , pixelMatrix
  , widthMatrix
  , (&!)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Data.Array
import qualified Data.List                                   as List
import qualified Data.Set                                    as Set

-- TYPES & INSTANCES

newtype Matrix a
  = Matrix { pixels :: Array Coordinates a }
  deriving stock (Show)

instance Functor Matrix where
  fmap f (Matrix pxs) = Matrix (fmap f pxs)

-- EXPORTED FUNCTIONS & OPERATORS

infixl 9 &!
(&!) ∷ Matrix a → Coordinates → Maybe a
m &! coord
  | inRangeMatrix coord m = Just $ pixelMatrix coord m
  | otherwise            = Nothing

newMatrix ∷ Coordinates → [(Coordinates, a)] → Matrix a
newMatrix (width, height) = Matrix . array ((0, 0), (width - 1, height - 1))

widthMatrix ∷ Matrix a → Int
widthMatrix = fst . dimensionsMatrix

heightMatrix ∷ Matrix a → Int
heightMatrix = snd . dimensionsMatrix

inRangeMatrix ∷ Coordinates → Matrix a → Bool
inRangeMatrix coord (Matrix pxs) = inRange (bounds pxs) coord

pixelMatrix ∷ Coordinates → Matrix a → a
pixelMatrix coord (Matrix pxs) = pxs ! coord

discoverBlock ∷ Eq a ⇒ Matrix a → Coordinates → Block
discoverBlock m startPos = Set.toList $ go Set.empty startPos where
  targetColor = m &! startPos

  go visited pos
    | pos `Set.member` visited = visited
    | m &! pos /= targetColor  = visited
    | otherwise                = List.foldl' go (Set.insert pos visited) (neighbours pos)

-- UTILS (PRIVATE)

dimensionsMatrix ∷ Matrix a → Coordinates
dimensionsMatrix (Matrix pxs) = (maxX + 1, maxY + 1) where
  (_, (maxX, maxY)) = bounds pxs
