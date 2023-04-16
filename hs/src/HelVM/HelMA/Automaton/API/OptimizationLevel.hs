module HelVM.HelMA.Automaton.API.OptimizationLevel where

import           HelVM.HelIO.SwitchEnum

import           HelVM.HelIO.Containers.LLIndexSafe

-- | Constructors

fromBool :: Bool -> OptimizationLevel
fromBool = enumFromBool

fromNatural :: Natural -> OptimizationLevel
fromNatural = fromMaybe AllOptimizations . indexMaybe optimizationLevels . fromIntegral

defaultOptimizationLevel :: OptimizationLevel
defaultOptimizationLevel = defaultEnum

optimizationLevels :: [OptimizationLevel]
optimizationLevels = generateEnums 4

-- | Types

data OptimizationLevel =
    NoOptimizations
  | BasicOptimizations
  | SomeOptimizations
  | AllOptimizations
  deriving stock (Bounded , Enum , Eq , Read , Show)
