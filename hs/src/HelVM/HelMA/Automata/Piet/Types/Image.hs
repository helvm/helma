module HelVM.HelMA.Automata.Piet.Types.Image
  ( Image (..)
  , dimensionsImage
  , discoverBlock
  , heightImage
  , inRangeImage
  , newImage
  , pixelImage
  , widthImage
  , (&!)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Data.Array.Diff
import qualified Data.List                                   as List
import qualified Data.Set                                    as Set

-- TYPES & INSTANCES

newtype Image a
  = Image { pixels :: DiffArray Coordinates a }
  deriving stock (Show)

instance Functor Image where
  fmap f (Image pxs) = Image (amap f pxs)

-- OPERATORS

infixl 9 &!
(&!) ∷ Image a → Coordinates → Maybe a
m &! coord
  | inRangeImage coord m = Just $ pixelImage coord m
  | otherwise            = Nothing

-- INITIALIZERS & CONSTRUCTORS

newImage ∷ Coordinates → [(Coordinates, a)] → Image a
newImage (width, height) = Image . array ((0, 0), (width - 1, height - 1))

-- GETTERS & QUERIES

dimensionsImage ∷ Image a → Coordinates
dimensionsImage (Image pxs) = (maxX + 1, maxY + 1) where
  (_, (maxX, maxY)) = bounds pxs

widthImage ∷ Image a → Int
widthImage = fst . dimensionsImage

heightImage ∷ Image a → Int
heightImage = snd . dimensionsImage

inRangeImage ∷ Coordinates → Image a → Bool
inRangeImage coord (Image pxs) = inRange (bounds pxs) coord

pixelImage ∷ Coordinates → Image a → a
pixelImage coord (Image pxs) = pxs ! coord

-- GRAPH / IMAGE SEARCH

discoverBlock ∷ Eq a ⇒ Image a → Coordinates → Block
discoverBlock m startPos = Set.toList $ go Set.empty startPos where
  targetColor = m &! startPos

  go visited pos
    | pos `Set.member` visited  = visited
    | m &! pos /= targetColor = visited
    | otherwise               = List.foldl' go (Set.insert pos visited) (neighbours pos)

