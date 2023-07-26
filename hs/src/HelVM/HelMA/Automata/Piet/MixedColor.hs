module HelVM.HelMA.Automata.Piet.MixedColor where

import           HelVM.HelMA.Automata.Piet.Common.EnumExtra

-- | Types

mixedColor :: MixedColor -> MixedColor -> MixedColor
mixedColor (MixedColor b1 h1) (MixedColor b2 h2) = MixedColor (sumEnums b1 b2) (sumEnums h1 h2)

data MixedColor = MixedColor !Brightness !Hue
  deriving stock (Show , Read , Eq , Ord)

data Brightness = Light | Normal | Dark
  deriving stock (Bounded , Show , Read, Eq , Ord , Enum)

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta
  deriving stock (Bounded , Show , Read, Eq , Ord , Enum)
