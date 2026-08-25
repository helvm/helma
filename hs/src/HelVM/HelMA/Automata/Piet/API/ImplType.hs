module HelVM.HelMA.Automata.Piet.API.ImplType where

import           Data.Default

defaultImplType ∷ ImplType
defaultImplType = def

fileImplTypes ∷ NonEmpty ImplType
fileImplTypes = universeNonEmpty

data ImplType
  = StepState
  | Collision
  deriving stock (Bounded, Enum, Eq, Read, Show)

instance Default ImplType where
  def = minBound
