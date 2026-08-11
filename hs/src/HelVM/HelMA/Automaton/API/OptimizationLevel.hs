module HelVM.HelMA.Automaton.API.OptimizationLevel where

import           HelVM.HelIO.Containers.MTIndexSafe

-- | Constructors

fromNatural ∷ Natural → OptimizationLevel
fromNatural = fromInt . fromIntegral

fromInt ∷ Int → OptimizationLevel
fromInt = fromMaybe maxBound . indexMaybe (toList optimizationLevels)

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
