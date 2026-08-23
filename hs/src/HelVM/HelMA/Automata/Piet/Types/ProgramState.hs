module HelVM.HelMA.Automata.Piet.Types.ProgramState
  ( ProgramState (..)
  , collisionCount
  , ic
  , initialState
  , stack
  ) where

import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter

import           Lens.Micro.Platform

initialState ∷ ProgramState
initialState = ProgramState
  { _ic             = initialInstructionCounter
  , _stack          = []
  , _collisionCount = 0
  }

data ProgramState
  = ProgramState
      { _ic             :: !InstructionCounter
      , _stack          :: ![Int]
      , _collisionCount :: !Int
      }
  deriving stock (Eq, Show)

makeLenses ''ProgramState
