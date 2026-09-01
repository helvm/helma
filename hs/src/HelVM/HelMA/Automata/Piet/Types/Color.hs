module HelVM.HelMA.Automata.Piet.Types.Color
  ( Color (..)
  , hueSteps
  , lightnessSteps
  , pixelToColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Codec.Picture

lightnessSteps ∷ Color → Color → Maybe Int
lightnessSteps (Chromatic (ChromaticColor l1 _)) (Chromatic (ChromaticColor l2 _)) = Just $ diffLightness l1 l2
lightnessSteps _ _                                                                 = Nothing

hueSteps ∷ Color → Color → Maybe Int
hueSteps (Chromatic (ChromaticColor _ h1)) (Chromatic (ChromaticColor _ h2)) = Just $ diffHue h1 h2
hueSteps _ _                                                                 = Nothing

pixelToColor ∷ PixelRGB8 → Color
pixelToColor (PixelRGB8 r g b) = rgb2Color r g b

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
