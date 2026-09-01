module HelVM.HelMA.Automata.Piet.Types.Color
  ( Color (..)
  , pixelToColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Codec.Picture

pixelToColor ∷ PixelRGB8 → Color
pixelToColor (PixelRGB8 r g b) = rgb2Color r g b

rgb2Color ∷ (Num w, Eq w) ⇒ w → w → w → Color
rgb2Color 0x00 0x00 0x00 = Black
rgb2Color 0xff 0xff 0xff = White
rgb2Color 0xff 0xc0 0xc0 = Chromatic (ChromaticColor Red     Light )
rgb2Color 0xff 0x00 0x00 = Chromatic (ChromaticColor Red     Normal)
rgb2Color 0xc0 0x00 0x00 = Chromatic (ChromaticColor Red     Dark  )
rgb2Color 0xff 0xff 0xc0 = Chromatic (ChromaticColor Yellow  Light )
rgb2Color 0xff 0xff 0x00 = Chromatic (ChromaticColor Yellow  Normal)
rgb2Color 0xc0 0xc0 0x00 = Chromatic (ChromaticColor Yellow  Dark  )
rgb2Color 0xc0 0xff 0xc0 = Chromatic (ChromaticColor Green   Light )
rgb2Color 0x00 0xff 0x00 = Chromatic (ChromaticColor Green   Normal)
rgb2Color 0x00 0xc0 0x00 = Chromatic (ChromaticColor Green   Dark  )
rgb2Color 0xc0 0xff 0xff = Chromatic (ChromaticColor Cyan    Light )
rgb2Color 0x00 0xff 0xff = Chromatic (ChromaticColor Cyan    Normal)
rgb2Color 0x00 0xc0 0xc0 = Chromatic (ChromaticColor Cyan    Dark  )
rgb2Color 0xc0 0xc0 0xff = Chromatic (ChromaticColor Blue    Light )
rgb2Color 0x00 0x00 0xff = Chromatic (ChromaticColor Blue    Normal)
rgb2Color 0x00 0x00 0xc0 = Chromatic (ChromaticColor Blue    Dark  )
rgb2Color 0xff 0xc0 0xff = Chromatic (ChromaticColor Magenta Light )
rgb2Color 0xff 0x00 0xff = Chromatic (ChromaticColor Magenta Normal)
rgb2Color 0xc0 0x00 0xc0 = Chromatic (ChromaticColor Magenta Dark  )
rgb2Color _    _    _    = White

data Color
  = Chromatic !ChromaticColor
  | White
  | Black
  deriving stock (Eq, Ord, Read, Show)
