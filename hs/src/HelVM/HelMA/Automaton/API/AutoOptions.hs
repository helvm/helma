module HelVM.HelMA.Automaton.API.AutoOptions where

import           HelVM.HelMA.Automaton.Trampoline
import           HelVM.HelMA.Automaton.Types.DumpType

defaultDebug ∷ Bool
defaultDebug = False

fastAutoOptions ∷ AutoOptions
fastAutoOptions = AutoOptions
  { limit    = Nothing
  , dumpType = No
  , debug    = False
  }

simpleAutoOptions ∷ AutoOptions
simpleAutoOptions = AutoOptions
  { limit    = testMaybeLimit
  , dumpType = Pretty
  , debug    = True
  }

data AutoOptions
  = AutoOptions
      { limit    :: LimitMaybe
      , dumpType :: DumpType
      , debug    :: !Bool
      }
