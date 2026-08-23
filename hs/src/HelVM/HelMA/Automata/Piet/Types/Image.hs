module HelVM.HelMA.Automata.Piet.Types.Image
  ( Image (..)
  , dimensionsImage
  , discoverBlock
  , heightImage
  , inRangeImage
  , newImage
  , pixelImage
  , setPixelImage
  , widthImage
  , (&!)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Data.Array.Diff
import qualified Data.List                                   as List
import qualified Data.Set                                    as Set

infixl 9 &!
(&!) ∷ Image a → Coordinates → Maybe a
m &! coord
  | inRangeImage coord m = Just $ pixelImage coord m
  | otherwise            = Nothing

widthImage ∷ Image a → Int
widthImage = fst . dimensionsImage

heightImage ∷ Image a → Int
heightImage = snd . dimensionsImage

dimensionsImage ∷ Image a → Coordinates
dimensionsImage (Image pixels) = (maxX + 1, maxY + 1) where
  (_, (maxX, maxY)) = bounds pixels

inRangeImage ∷ Coordinates → Image a → Bool
inRangeImage (x, y) img = 0 <= x && x < widthImage img && 0 <= y && y < heightImage img

pixelImage ∷ Coordinates → Image a → a
pixelImage coord (Image pixels) = pixels ! coord

setPixelImage ∷ Coordinates → a → Image a → Image a
setPixelImage coord pixel img = img { pixels = pixels img // [(coord, pixel)] }

newImage ∷ Coordinates → [(Coordinates, a)] → Image a
newImage (width, height) = Image . array ((0, 0), (width - 1, height - 1))

instance Functor Image where
  fmap f img = img { pixels = amap f (pixels img) }

newtype Image a
  = Image { pixels :: DiffArray Coordinates a }
  deriving stock (Show)

discoverBlock ∷ Eq a ⇒ Image a → Coordinates → Block
discoverBlock m startPos = Set.toList $ go Set.empty startPos where
  targetColor = m &! startPos

  go visited pos
    | pos `Set.member` visited    = visited
    | m &! pos /= targetColor   = visited
    | otherwise                 = List.foldl' go (Set.insert pos visited) (neighbours pos)
