module HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy
  ( MulticoloredCodelStrategy (..)
  ) where

data MulticoloredCodelStrategy
  = MulticoloredCodelAsWhite
  | MulticoloredCodelAsBlack
  | MulticoloredCodelCenter
  | MulticoloredCodelModal
  | MulticoloredCodelAverage
  deriving stock (Eq, Ord, Show)
