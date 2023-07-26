module HelVM.HelMA.Automata.Piet.InterpreterStatus where

import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.MovePointer

data InterpreterStatus = InterpreterStatus
    { dp       :: DirectionPointer
    , cc       :: CodelChooser
    , position :: Coordinates
--    , stack    :: RollStack Int
    }
    deriving stock (Show)
