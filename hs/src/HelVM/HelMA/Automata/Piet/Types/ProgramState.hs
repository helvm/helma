module HelVM.HelMA.Automata.Piet.Types.ProgramState
  ( ProgramState (..)
  , codelChooser
  , collisionCount
  , currentPosition
  , directionPointer
  , initialState
  , stack
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer


import           Lens.Micro.TH

initialState ∷ ProgramState
initialState = ProgramState
  { _directionPointer = DPRight
  , _codelChooser     = CCLeft
  , _currentPosition  = (0, 0)
  , _stack            = []
  , _collisionCount   = 0
  }

data ProgramState
  = ProgramState
      { _directionPointer :: DirectionPointer
      , _codelChooser     :: CodelChooser
      , _currentPosition  :: Coordinates
      , _stack            :: [Int]
      , _collisionCount   :: Int
      }
  deriving stock (Eq, Show)

makeLenses ''ProgramState
