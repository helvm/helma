module HelVM.HelMA.Automata.Piet.LLVM.ImageReader
  ( AdditionalColorStrategy (..)
  , CodelSizeMaybe
  , ImageConfig (..)
  , Matrix
  , MulticoloredCodelStrategy (..)
  , imageToCodels
  , readCodels
  , rgbImageToCodels
  ) where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelMA.Automata.Piet.MatrixBuilder
import           HelVM.HelMA.Automata.Piet.ToRGB8

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Codec.Picture

readCodels ∷ MonadSafe m ⇒ ImageConfig → DynamicImage → m (Matrix Color)
readCodels = imageToCodels

imageToCodels ∷ MonadSafe m ⇒ ImageConfig → DynamicImage → m (Matrix Color)
imageToCodels config image = toRGB8ImageM image >>= rgbImageToCodels config

rgbImageToCodels ∷ MonadSafe m ⇒ ImageConfig → Image PixelRGB8 → m (Matrix Color)
rgbImageToCodels config image = checkDimensions (modX, modY) $> buildMatrix (codelWidth, codelHeight) config codelSizeInt image where
  (codelWidth, modX) = divMod pixelWidth codelSizeInt
  (codelHeight, modY) = divMod pixelHeight codelSizeInt
  codelSizeInt = getIntCodelSize (pixelWidth, pixelHeight) image (codelSize config)
  pixelWidth = imageWidth image
  pixelHeight = imageHeight image

checkDimensions ∷ MonadSafe m ⇒ Coordinates → m ()
checkDimensions (0, 0) = pass
checkDimensions _      = liftError "CodelSizeError"
