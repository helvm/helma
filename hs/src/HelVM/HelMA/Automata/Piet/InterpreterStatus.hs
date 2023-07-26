module HelVM.HelMA.Automata.Piet.InterpreterStatus where

import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.MovePointer

emptyInterpreterStatus :: InterpreterStatus
emptyInterpreterStatus = InterpreterStatus
  { mp       = emptyMovePointer
  , position = emptyCoordinates
  , stack    = empty
  }

-- | Types

data InterpreterStatus = InterpreterStatus
  { mp       :: MovePointer
  , position :: Coordinates
  , stack    :: [Int]
  }
  deriving stock (Show)
