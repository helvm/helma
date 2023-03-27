module HelVM.HelMA.Automata.ETA.SimpleParams where

import           HelVM.HelMA.Automata.ETA.API.ETAImplType

import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Types.StackType

simpleParams :: ETAImplType -> StackType-> Source -> SimpleParams
simpleParams it st s = SimpleParams
  { implType   = it
  , source     = s
  , stackType  = st
  , autoOptions = simpleAutoParams
  }

-- | Type
data SimpleParams = SimpleParams
  { implType    :: !ETAImplType
  , source      :: !Source
  , stackType   :: !StackType
  , autoOptions :: !AutoOptions
  }
