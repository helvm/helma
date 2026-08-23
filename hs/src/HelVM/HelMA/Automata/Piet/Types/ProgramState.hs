module HelVM.HelMA.Automata.Piet.Types.ProgramState
  ( ProgramState (..)
  , collisionCount
  , im
  , initialState
  , stack
  ) where

import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Program

import           Lens.Micro.Platform

initialState ∷ Program → ProgramState
initialState p = ProgramState
  { _im             = initialInstructionMemory p
  , _stack          = []
  , _collisionCount = 0
  }

data ProgramState
  = ProgramState
      { _im             :: !InstructionMemory
      , _stack          :: ![Int]
      , _collisionCount :: !Int
      }

makeLenses ''ProgramState
