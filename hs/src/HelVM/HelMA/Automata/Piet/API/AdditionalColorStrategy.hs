module HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
  ( AdditionalColorStrategy (..)
  ) where

data AdditionalColorStrategy
  = AsWhite
  | AsBlack
  | Nearest
  deriving stock (Eq, Ord, Show)
