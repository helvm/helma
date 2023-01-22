module HelVM.HelMA.Automaton.API.AutoParams where

import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

verySimpleAutoParams :: AutoParams
verySimpleAutoParams = simpleAutoParams False

simpleAutoParams :: Bool -> AutoParams
simpleAutoParams c = AutoParams
  { compile = c
  , limit = testMaybeLimit
  , dumpType = Pretty
  }

data AutoParams = AutoParams
  { compile  :: Bool
  , limit    :: LimitMaybe
  , dumpType :: DumpType
  }
