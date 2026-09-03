module HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy
  ( MulticoloredCodelStrategy (..)
  ) where

data MulticoloredCodelStrategy
  = AsWhite
  | AsBlack
  | Center
  | Modal
  | Average
  deriving stock (Eq, Ord, Show)
