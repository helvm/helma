module HelVM.HelMA.Automata.Piet.Types.Image (
  Image,
  imgWidth,
  imgHeight,
  imgInRange,
  imgNew,
  imgPixel,
  imgSetPixel,
) where

import           Data.Array.Diff
import           HelVM.HelMA.Automata.Piet.Types.Coordinates

imgInRange :: Coordinates -> Image a -> Bool
imgInRange (x, y) img = 0 <= x && x < imgWidth img && 0 <= y && y < imgHeight img

imgPixel :: Coordinates -> Image a -> a
imgPixel (x, y) img = imgPixels img ! (x, y)

imgSetPixel :: Coordinates -> a -> Image a -> Image a
imgSetPixel (x, y) pixel img = img { imgPixels = imgPixels img // [((x, y), pixel)] }

imgNew :: Coordinates -> [(Coordinates, a)] -> Image a
imgNew (width, height) entries = Image width height $ array ((0, 0), (width - 1, height - 1)) entries

instance Functor Image where
  fmap f img = img { imgPixels = amap f (imgPixels img) }

data Image a = Image
  { imgWidth  :: !Int
  , imgHeight :: !Int
  , imgPixels :: !(DiffArray Coordinates a)
  }
  deriving stock (Show)
