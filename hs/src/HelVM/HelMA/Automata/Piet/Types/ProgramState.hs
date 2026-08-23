module HelVM.HelMA.Automata.Piet.Types.ProgramState
  ( ProgramState (..)
  , collisionCount
  , currentPosition
  , initialState
  , orientation
  , stack
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Orientation

import           Lens.Micro.Platform

initialState ∷ ProgramState
initialState = ProgramState
  { _orientation     = initialOrientation
  , _currentPosition = (0, 0)
  , _stack           = []
  , _collisionCount  = 0
  }

data ProgramState
  = ProgramState
      { _orientation     :: !Orientation
      , _currentPosition :: !Coordinates
      , _stack           :: ![Int]
      , _collisionCount  :: !Int
      }
  deriving stock (Eq, Show)

makeLenses ''ProgramState
