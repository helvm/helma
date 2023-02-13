module HelVM.HelMA.Automata.ETA.SimpleParams where

import           HelVM.HelMA.Automata.ETA.API.ETAImplType

import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Types.StackType

simpleParams :: ETAImplType -> StackType -> Bool -> Source -> SimpleParams
simpleParams it st c s = SimpleParams
  { implType   = it
  , source     = s
  , stackType  = st
  , autoOptions = simpleAutoParams c
  }

-- | Type
data SimpleParams = SimpleParams
  { implType    :: !ETAImplType
  , source      :: !Source
  , stackType   :: !StackType
  , autoOptions :: !AutoOptions
  }