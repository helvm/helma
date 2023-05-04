module HelVM.HelMA.Automaton.API.AutoOptions where

import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Trampoline
import           HelVM.HelMA.Automaton.Types.DumpType

simpleAutoParams :: AutoOptions
simpleAutoParams = AutoOptions
  { optLevel     = AllOptimizations
  , limit        = testMaybeLimit
  , dumpType     = Pretty
  }

data AutoOptions = AutoOptions
  { optLevel :: OptimizationLevel
  , limit    :: LimitMaybe
  , dumpType :: DumpType
  }
