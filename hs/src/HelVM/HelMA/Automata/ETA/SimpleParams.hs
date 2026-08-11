module HelVM.HelMA.Automata.ETA.SimpleParams where

import           HelVM.HelMA.Automata.ETA.API.AutomatonType

import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Types.StackType

simpleParams ∷ AutomatonType → StackType→ Source → SimpleParams
simpleParams it st s = SimpleParams
  { implType   = it
  , source     = s
  , stackType  = st
  , autoOptions = simpleAutoParams
  }

-- | Type
data SimpleParams
  = SimpleParams
      { implType    :: !AutomatonType
      , source      :: !Source
      , stackType   :: !StackType
      , autoOptions :: !AutoOptions
      }
