module HelVM.HelMA.Automata.Piet.LLVM.CompileOption
  ( OptimizationLevel (..)
  ) where

data OptimizationLevel
  = NoOptimization -- ^ -O0
  | OptimizationLevelLow -- ^ -O1
  | OptimizationLevelMiddle -- ^ -O2
  | OptimizationLevelHigh -- ^ -O3
  | SizeLevelLow -- ^ -Os
  | SizeLevelHigh -- ^ -Oz
  deriving stock (Eq, Show)
