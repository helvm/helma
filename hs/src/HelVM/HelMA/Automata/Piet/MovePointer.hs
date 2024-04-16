module HelVM.HelMA.Automata.Piet.MovePointer where

import           HelVM.HelMA.Automata.Piet.Common.EnumExtra

rotateMovePointer :: MovePointer -> MovePointer
rotateMovePointer (dp , cc) = (rotateEnum 1 dp , rotateEnum 1 cc)

-- | Constructors

emptyMovePointer :: MovePointer
emptyMovePointer = (DPRight , CCLeft)

-- | Types

type MovePointer = (DirectionPointer , CodelChooser)

data DirectionPointer = DPRight | DPDown | DPLeft  | DPUp
  deriving stock (Bounded , Show, Read, Eq, Ord, Enum)

data CodelChooser = CCLeft | CCRight
  deriving stock (Bounded , Show, Read, Eq, Ord, Enum)
