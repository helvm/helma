module HelVM.HelMA.Automata.Piet.Color where

import           HelVM.HelIO.SwitchEnum

mixedColor :: MixedColor -> MixedColor -> MixedColor
mixedColor (MixedColor b1 h1) (MixedColor b2 h2) = MixedColor (brightnessChange b1 b2) (hueChange h1 h2)

brightnessChange :: Brightness -> Brightness -> Brightness
brightnessChange = change 3

hueChange :: Hue -> Hue -> Hue
hueChange = change 6

change :: (Bounded e, Enum e) => Int -> e -> e -> e
change i e1 e2 = unsafeEnum $ (fromEnum e2 - fromEnum e1) `mod` i

-- | Constructors
rgb2Color :: (Num a , Eq a) => RGBColor a -> Color
rgb2Color (RGBColor 0xff 0xc0 0xc0) = OtherColor $ MixedColor Light  Red
rgb2Color (RGBColor 0xff 0x00 0x00) = OtherColor $ MixedColor Normal Red
rgb2Color (RGBColor 0xc0 0x00 0x00) = OtherColor $ MixedColor Dark   Red
rgb2Color (RGBColor 0xff 0xff 0xc0) = OtherColor $ MixedColor Light  Yellow
rgb2Color (RGBColor 0xff 0xff 0x00) = OtherColor $ MixedColor Normal Yellow
rgb2Color (RGBColor 0xc0 0xc0 0x00) = OtherColor $ MixedColor Dark   Yellow
rgb2Color (RGBColor 0xc0 0xff 0xc0) = OtherColor $ MixedColor Light  Green
rgb2Color (RGBColor 0x00 0xff 0x00) = OtherColor $ MixedColor Normal Green
rgb2Color (RGBColor 0x00 0xc0 0x00) = OtherColor $ MixedColor Dark   Green
rgb2Color (RGBColor 0xc0 0xff 0xff) = OtherColor $ MixedColor Light  Cyan
rgb2Color (RGBColor 0x00 0xff 0xff) = OtherColor $ MixedColor Normal Cyan
rgb2Color (RGBColor 0x00 0xc0 0xc0) = OtherColor $ MixedColor Dark   Cyan
rgb2Color (RGBColor 0xc0 0xc0 0xff) = OtherColor $ MixedColor Light  Blue
rgb2Color (RGBColor 0x00 0x00 0xff) = OtherColor $ MixedColor Normal Blue
rgb2Color (RGBColor 0x00 0x00 0xc0) = OtherColor $ MixedColor Dark   Blue
rgb2Color (RGBColor 0xff 0xc0 0xff) = OtherColor $ MixedColor Light  Magenta
rgb2Color (RGBColor 0xff 0x00 0xff) = OtherColor $ MixedColor Normal Magenta
rgb2Color (RGBColor 0xc0 0x00 0xc0) = OtherColor $ MixedColor Dark   Magenta
rgb2Color (RGBColor 0x00 0x00 0x00) = Black
rgb2Color (RGBColor 0xff 0xff 0xff) = White
rgb2Color  _                        = White

-- | Types
data RGBColor a = RGBColor a a a

data Color = Black | White | OtherColor !MixedColor
  deriving stock (Show , Read , Eq , Ord)

data MixedColor = MixedColor !Brightness !Hue
  deriving stock (Show , Read , Eq , Ord)

data Brightness = Light | Normal | Dark
  deriving stock (Bounded , Show , Read, Eq , Ord , Enum)

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta
  deriving stock (Bounded , Show , Read, Eq , Ord , Enum)
