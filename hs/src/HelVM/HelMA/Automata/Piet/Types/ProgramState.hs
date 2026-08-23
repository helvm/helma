module HelVM.HelMA.Automata.Piet.Types.ProgramState
  ( ProgramState (..)
  , collisionCount
  , initialState
  , memory
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program

import           Lens.Micro.Platform

initialState ∷ Program → ProgramState
initialState p = ProgramState
  { _memory         = initialMemory p
  , _collisionCount = 0
  }

data ProgramState
  = ProgramState
      { _memory         :: !Memory
      , _collisionCount :: !Int
      }

makeLenses ''ProgramState
