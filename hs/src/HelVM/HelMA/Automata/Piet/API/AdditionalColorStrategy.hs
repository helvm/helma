module HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
  ( AdditionalColorStrategy (..)
  ) where

data AdditionalColorStrategy
  = AdditionalColorAsWhite
  | AdditionalColorAsBlack
  | AdditionalColorNearest
  deriving stock (Eq, Ord, Show)
