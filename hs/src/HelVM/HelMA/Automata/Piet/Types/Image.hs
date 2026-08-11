module HelVM.HelMA.Automata.Piet.Types.Image
  ( Image (..)
  , heightImage
  , inRangeImage
  , newImage
  , pixelImage
  , setPixelImage
  , witdthImage
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Data.Array.Diff

witdthImage ∷ Image a → Int
witdthImage = fst . dimensionsImage

heightImage ∷ Image a → Int
heightImage = snd . dimensionsImage

dimensionsImage ∷ Image a → Coordinates
dimensionsImage (Image pixels) = (maxX - minX + 1, maxY - minY + 1) where ((minX, minY), (maxX, maxY)) = bounds pixels

inRangeImage ∷ Coordinates → Image a → Bool
inRangeImage (x, y) img = 0 <= x && x < witdthImage img && 0 <= y && y < heightImage img

pixelImage ∷ Coordinates → Image a → a
pixelImage (x, y) img = pixels img ! (x, y)

setPixelImage ∷ Coordinates → a → Image a → Image a
setPixelImage (x, y) pixel img = img { pixels = pixels img // [((x, y), pixel)] }

newImage ∷ Coordinates → [(Coordinates, a)] → Image a
newImage (width, height) = Image . array ((0, 0), (width - 1, height - 1))

instance Functor Image where
  fmap f img = img { pixels = amap f (pixels img) }

newtype Image a
  = Image { pixels :: DiffArray Coordinates a }
  deriving stock (Show)
