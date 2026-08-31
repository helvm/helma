module HelVM.HelMA.Automata.Piet.Types.Matrix
  ( Matrix (..)
  , discoverBlock
  , heightImage
  , inRangeImage
  , newImage
  , pixelImage
  , widthImage
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
  | inRangeImage coord m = Just $ pixelImage coord m
  | otherwise            = Nothing

newImage ∷ Coordinates → [(Coordinates, a)] → Matrix a
newImage (width, height) = Matrix . array ((0, 0), (width - 1, height - 1))

widthImage ∷ Matrix a → Int
widthImage = fst . dimensionsImage

heightImage ∷ Matrix a → Int
heightImage = snd . dimensionsImage

inRangeImage ∷ Coordinates → Matrix a → Bool
inRangeImage coord (Matrix pxs) = inRange (bounds pxs) coord

pixelImage ∷ Coordinates → Matrix a → a
pixelImage coord (Matrix pxs) = pxs ! coord

discoverBlock ∷ Eq a ⇒ Matrix a → Coordinates → Block
discoverBlock m startPos = Set.toList $ go Set.empty startPos where
  targetColor = m &! startPos

  go visited pos
    | pos `Set.member` visited = visited
    | m &! pos /= targetColor  = visited
    | otherwise                = List.foldl' go (Set.insert pos visited) (neighbours pos)

-- UTILS (PRIVATE)

dimensionsImage ∷ Matrix a → Coordinates
dimensionsImage (Matrix pxs) = (maxX + 1, maxY + 1) where
  (_, (maxX, maxY)) = bounds pxs
