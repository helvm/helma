module HelVM.HelMA.Automaton.API.OptimizationLevel where

import           HelVM.HelIO.SwitchEnum

import           HelVM.HelIO.Containers.LLIndexSafe

-- | Constructors

--TODO to remove
fromBool :: Bool -> OptimizationLevel
fromBool = enumFromBool

fromNatural :: Natural -> OptimizationLevel
fromNatural = fromMaybe AllOptimizations . indexMaybe (toList optimizationLevels) . fromIntegral

defaultOptimizationLevel :: OptimizationLevel
defaultOptimizationLevel = minBound

optimizationLevels :: NonEmpty OptimizationLevel
optimizationLevels = universeNonEmpty

-- | Types

data OptimizationLevel =
    NoOptimizations
  | BasicOptimizations
  | SomeOptimizations
  | AllOptimizations
  deriving stock (Bounded , Enum , Eq , Read , Show)
