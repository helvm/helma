module HelVM.HelMA.Automata.Piet.InterpreterStatus where

import qualified HelVM.HelMA.Automata.Piet.Coordinates as C
--import           HelVM.HelMA.Automata.Piet.DirectionPointer
import           HelVM.HelMA.Automata.Piet.MovePointer hiding (rotateMovePointer, setPosition)
import qualified HelVM.HelMA.Automata.Piet.MovePointer as MP

-- UnitControl?

nextInterpreterStatus is = nextInterpreterStatus' (isBlocked xy') xy' where xy' = addCoordinates is

nextInterpreterStatus' True  _  = rotateMovePointer
nextInterpreterStatus' False xy = setPosition

rotateMovePointer :: InterpreterStatus -> InterpreterStatus
rotateMovePointer is = is { mp = mp' } where mp' = MP.rotateMovePointer $ mp is

setPosition :: C.Coordinates -> InterpreterStatus -> InterpreterStatus
setPosition xy is = is {position = xy}

addCoordinates :: InterpreterStatus -> C.Coordinates
addCoordinates = liftA2 C.addCoordinates getDP position

getDP :: InterpreterStatus -> DirectionPointer
getDP = fst . mp

-- Constructors

emptyInterpreterStatus :: InterpreterStatus
emptyInterpreterStatus = InterpreterStatus
  { mp       = emptyMovePointer
  , position = C.emptyCoordinates -- InstructionCounter
  , stack    = empty              -- memoryStack
                                  -- ControlMemory
  }

-- | Types

data InterpreterStatus = InterpreterStatus
  { mp       :: MovePointer
  , position :: C.Coordinates
  , stack    :: [Int]
  }
  deriving stock (Show)
