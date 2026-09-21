module HelVM.HelMA.Automaton.API.AutoOptions where

import           HelVM.HelMA.Automaton.Trampoline
import           HelVM.HelMA.Automaton.Types.DumpType

fastAutoOptions ∷ AutoOptions
fastAutoOptions = AutoOptions
  { limit        = Nothing
  , dumpType     = Pretty
  }

simpleAutoOptions ∷ AutoOptions
simpleAutoOptions = AutoOptions
  { limit        = testMaybeLimit
  , dumpType     = Pretty
  }

data AutoOptions
  = AutoOptions
      { limit    :: LimitMaybe
      , dumpType :: DumpType
      }
