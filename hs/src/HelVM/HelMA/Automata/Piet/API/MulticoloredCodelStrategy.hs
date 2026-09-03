module HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy where

import           Data.Default

defaultMulticoloredCodelStrategy ∷ MulticoloredCodelStrategy
defaultMulticoloredCodelStrategy = def

fileMulticoloredCodelStrategies ∷ NonEmpty MulticoloredCodelStrategy
fileMulticoloredCodelStrategies = universeNonEmpty

data MulticoloredCodelStrategy
  = AsWhite
  | AsBlack
  | Center
  | Modal
  | Average
  deriving stock (Bounded, Enum, Eq, Read, Show)

instance Default MulticoloredCodelStrategy where
  def = minBound
