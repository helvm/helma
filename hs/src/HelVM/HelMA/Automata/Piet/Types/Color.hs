module HelVM.HelMA.Automata.Piet.Types.Color (
  rgba2Color,
  Color(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.Brightness
import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue

rgba2Color :: (Num w, Eq w) => w -> w -> w -> Color
rgba2Color 0x00 0x00 0x00 = Black
rgba2Color 0xff 0xff 0xff = White
rgba2Color 0xff 0xc0 0xc0 = Chromatic (ChromaticColor Light  Red)
rgba2Color 0xff 0x00 0x00 = Chromatic (ChromaticColor Normal Red)
rgba2Color 0xc0 0x00 0x00 = Chromatic (ChromaticColor Dark   Red)
rgba2Color 0xff 0xff 0xc0 = Chromatic (ChromaticColor Light  Yellow)
rgba2Color 0xff 0xff 0x00 = Chromatic (ChromaticColor Normal Yellow)
rgba2Color 0xc0 0xc0 0x00 = Chromatic (ChromaticColor Dark   Yellow)
rgba2Color 0xc0 0xff 0xc0 = Chromatic (ChromaticColor Light  Green)
rgba2Color 0x00 0xff 0x00 = Chromatic (ChromaticColor Normal Green)
rgba2Color 0x00 0xc0 0x00 = Chromatic (ChromaticColor Dark   Green)
rgba2Color 0xc0 0xff 0xff = Chromatic (ChromaticColor Light  Cyan)
rgba2Color 0x00 0xff 0xff = Chromatic (ChromaticColor Normal Cyan)
rgba2Color 0x00 0xc0 0xc0 = Chromatic (ChromaticColor Dark   Cyan)
rgba2Color 0xc0 0xc0 0xff = Chromatic (ChromaticColor Light  Blue)
rgba2Color 0x00 0x00 0xff = Chromatic (ChromaticColor Normal Blue)
rgba2Color 0x00 0x00 0xc0 = Chromatic (ChromaticColor Dark   Blue)
rgba2Color 0xff 0xc0 0xff = Chromatic (ChromaticColor Light  Magenta)
rgba2Color 0xff 0x00 0xff = Chromatic (ChromaticColor Normal Magenta)
rgba2Color 0xc0 0x00 0xc0 = Chromatic (ChromaticColor Dark   Magenta)
rgba2Color _    _    _    = White

data Color = Black | White | Chromatic !ChromaticColor
  deriving stock (Show, Read, Eq, Ord)
