module HelVM.HelMA.Automaton.API.AutoOptions where

import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

verySimpleAutoParams :: AutoOptions
verySimpleAutoParams = simpleAutoParams False

simpleAutoParams :: Bool -> AutoOptions
simpleAutoParams c = AutoOptions
  { optLevel     = BasicOptimizations
  , compileFlag  = c
  , limit        = testMaybeLimit
  , dumpType     = Pretty
  }

data AutoOptions = AutoOptions
  { optLevel    :: OptimizationLevel
  , compileFlag :: Bool
  , limit       :: LimitMaybe
  , dumpType    :: DumpType
  }
