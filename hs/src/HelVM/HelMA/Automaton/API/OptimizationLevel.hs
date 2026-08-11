module HelVM.HelMA.Automaton.API.OptimizationLevel where

import           HelVM.HelIO.Containers.MTIndexSafe

-- | Constructors

optimizationLevelFromNatural ∷ Natural → OptimizationLevel
optimizationLevelFromNatural = optimizationLevelFromInt . fromIntegral

optimizationLevelFromInt ∷ Int → OptimizationLevel
optimizationLevelFromInt = fromInt optimizationLevels

defaultOptimizationLevel ∷ OptimizationLevel
defaultOptimizationLevel = minBound

optimizationLevels ∷ NonEmpty OptimizationLevel
optimizationLevels = universeNonEmpty

-- | Types

data OptimizationLevel
  = NoOptimizations
  | BasicOptimizations
  | SomeOptimizations
  | AllOptimizations
  deriving stock (Bounded, Enum, Eq, Read, Show)

----

fromInt ∷ (Bounded a, Foldable t) ⇒ t a → Int → a
fromInt l i = fromMaybe maxBound $ indexMaybe (toList l) i
