module HelVM.HelMA.Automata.Piet.Types.ProgramState
  ( ProgramState (..)
  , collisionCount
  , ic
  , initialState
  , program
  , stack
  ) where

import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.Program

import           Lens.Micro.Platform

initialState ∷ Program → ProgramState
initialState p = ProgramState
  { _program = p
  ,  _ic             = initialInstructionCounter
  , _stack          = []
  , _collisionCount = 0
  }

data ProgramState
  = ProgramState
      { _program        :: !Program
      , _ic             :: !InstructionCounter
      , _stack          :: ![Int]
      , _collisionCount :: !Int
      }
  deriving stock (Show)

makeLenses ''ProgramState
