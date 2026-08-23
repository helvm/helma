module HelVM.HelMA.Automata.Piet.Types.Color
  ( Color (..)
  , hueSteps
  , lightnessSteps
  , pixelToColor
  , rgb2Color
  , toRGB8
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

toRGB8 ∷ MonadSafe m ⇒ DynamicImage → m (Image PixelRGB8)
toRGB8 (ImageRGB8 rgb8)   = pure rgb8
toRGB8 (ImageRGBA8 rgba8) = pure $ convertRGB8 (ImageRGBA8 rgba8)
toRGB8 (ImageY8 _)        = liftError  "Y8 format"
toRGB8 (ImageY16 _)       = liftError  "Y16 format"
toRGB8 (ImageY32 _)       = liftError  "Y32 format"
toRGB8 (ImageYF _)        = liftError  "YF format"
toRGB8 (ImageYA8 _)       = liftError  "YA8 format"
toRGB8 (ImageYA16 _)      = liftError  "YA16 format"
toRGB8 (ImageRGB16 _)     = liftError  "RGB16 format"
toRGB8 (ImageRGBF _)      = liftError  "RGBF format"
toRGB8 (ImageRGBA16 _)    = liftError  "RGBA16 format"
toRGB8 (ImageYCbCr8 _)    = liftError  "YCbCr8 format"
toRGB8 (ImageCMYK8 _)     = liftError  "CMYK8 format"
toRGB8 (ImageCMYK16 _)    = liftError  "CMYK16 format"

pixelToColor ∷ PixelRGB8 → Color
pixelToColor (PixelRGB8 r g b) = rgb2Color r g b

lightnessSteps ∷ Color → Color → Maybe Int
lightnessSteps (Chromatic (ChromaticColor l1 _)) (Chromatic (ChromaticColor l2 _)) =
  Just $ (fromEnum l2 - fromEnum l1) `mod` 3
lightnessSteps _ _ = Nothing

hueSteps ∷ Color → Color → Maybe Int
hueSteps (Chromatic (ChromaticColor _ h1)) (Chromatic (ChromaticColor _ h2)) =
  Just $ (fromEnum h2 - fromEnum h1) `mod` 6
hueSteps _ _ = Nothing

rgb2Color ∷ (Num w, Eq w) ⇒ w → w → w → Color
rgb2Color 0x00 0x00 0x00 = Black
rgb2Color 0xff 0xff 0xff = White
rgb2Color 0xff 0xc0 0xc0 = Chromatic (ChromaticColor Light  Red)
rgb2Color 0xff 0x00 0x00 = Chromatic (ChromaticColor Normal Red)
rgb2Color 0xc0 0x00 0x00 = Chromatic (ChromaticColor Dark   Red)
rgb2Color 0xff 0xff 0xc0 = Chromatic (ChromaticColor Light  Yellow)
rgb2Color 0xff 0xff 0x00 = Chromatic (ChromaticColor Normal Yellow)
rgb2Color 0xc0 0xc0 0x00 = Chromatic (ChromaticColor Dark   Yellow)
rgb2Color 0xc0 0xff 0xc0 = Chromatic (ChromaticColor Light  Green)
rgb2Color 0x00 0xff 0x00 = Chromatic (ChromaticColor Normal Green)
rgb2Color 0x00 0xc0 0x00 = Chromatic (ChromaticColor Dark   Green)
rgb2Color 0xc0 0xff 0xff = Chromatic (ChromaticColor Light  Cyan)
rgb2Color 0x00 0xff 0xff = Chromatic (ChromaticColor Normal Cyan)
rgb2Color 0x00 0xc0 0xc0 = Chromatic (ChromaticColor Dark   Cyan)
rgb2Color 0xc0 0xc0 0xff = Chromatic (ChromaticColor Light  Blue)
rgb2Color 0x00 0x00 0xff = Chromatic (ChromaticColor Normal Blue)
rgb2Color 0x00 0x00 0xc0 = Chromatic (ChromaticColor Dark   Blue)
rgb2Color 0xff 0xc0 0xff = Chromatic (ChromaticColor Light  Magenta)
rgb2Color 0xff 0x00 0xff = Chromatic (ChromaticColor Normal Magenta)
rgb2Color 0xc0 0x00 0xc0 = Chromatic (ChromaticColor Dark   Magenta)
rgb2Color _    _    _    = White

data Color
  = Black
  | White
  | Chromatic !ChromaticColor
  deriving stock (Eq, Ord, Read, Show)
