module HelVM.HelMA.Automaton.API.AutomatonOptions where

import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

optLevelAutoOptions :: AutomatonOptions -> OptimizationLevel
optLevelAutoOptions = optLevel . autoOptions

withDefaultRam :: StackType -> AutoOptions -> AutomatonOptions
withDefaultRam s ao = AutomatonOptions
  { ramType     = defaultRAMType
  , stackType   = s
  , autoOptions = ao
  }

data AutomatonOptions = AutomatonOptions
  { stackType   :: StackType
  , ramType     :: RAMType
  , autoOptions :: AutoOptions
  }
