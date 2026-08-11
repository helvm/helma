-- | The types for codels.
module HelVM.HelMA.Automata.Piet.LLVM.Codel
  ( Codel (..)
  , Hue (..)
  , Lightness (..)
  ) where

data Codel
  = AchromaticCodel Hue Lightness
  | WhiteCodel
  | BlackCodel
  deriving stock (Eq, Ord, Show)

data Hue
  = Red
  | Yellow
  | Green
  | Cyan
  | Blue
  | Magenta
  deriving stock (Enum, Eq, Ord, Show)

data Lightness
  = Light
  | Normal
  | Dark
  deriving stock (Enum, Eq, Ord, Show)
