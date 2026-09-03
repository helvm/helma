module HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy where

import           Data.Default

defaultAdditionalColorStrategy ∷ AdditionalColorStrategy
defaultAdditionalColorStrategy = def

fileAdditionalColorStrategies ∷ NonEmpty AdditionalColorStrategy
fileAdditionalColorStrategies = universeNonEmpty

data AdditionalColorStrategy
  = AsWhite
  | AsBlack
  | Nearest
  deriving stock (Bounded, Enum, Eq, Read, Show)

instance Default AdditionalColorStrategy where
  def = minBound
