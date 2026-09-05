module HelVM.HelMA.Automaton.API.AutoOptions where

import           HelVM.HelMA.Automaton.Trampoline
import           HelVM.HelMA.Automaton.Types.DumpType

simpleAutoParams ∷ AutoOptions
simpleAutoParams = AutoOptions
  { limit        = testMaybeLimit
  , dumpType     = Pretty
  }

data AutoOptions
  = AutoOptions
      { limit    :: LimitMaybe
      , dumpType :: DumpType
      }
