module HelVM.HelMA.Automata.Piet.ImageReader
  ( readColors
  , rgbImageToColors
  ) where

import           HelVM.HelMA.Automata.Piet.MatrixBuilder
import           HelVM.HelMA.Automata.Piet.ToRGB8

import           HelVM.HelMA.Automata.Piet.API.ImageConfig

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

readColors ∷ MonadSafe m ⇒ ImageConfig → DynamicImage → m (Matrix Color)
readColors config image = rgbImageToColors config =<< toRGB8ImageM image

rgbImageToColors ∷ MonadSafe m ⇒ ImageConfig → Image PixelRGB8 → m (Matrix Color)
rgbImageToColors config image = checkDimensions (modX, modY) $> buildMatrix (codelWidth, codelHeight) config codelSizeInt image where
  (codelWidth, modX) = divMod pixelWidth codelSizeInt
  (codelHeight, modY) = divMod pixelHeight codelSizeInt
  codelSizeInt = getIntCodelSize (pixelWidth, pixelHeight) image (codelSize config)
  pixelWidth = imageWidth image
  pixelHeight = imageHeight image

checkDimensions ∷ MonadSafe m ⇒ Coordinates → m ()
checkDimensions (0, 0) = pass
checkDimensions _      = liftError "CodelSizeError"
