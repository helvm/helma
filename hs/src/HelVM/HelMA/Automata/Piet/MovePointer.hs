module HelVM.HelMA.Automata.Piet.MovePointer where

type MovePointer = (DirectionPointer , CodelChooser)

data DirectionPointer = DPRight | DPDown | DPLeft  | DPUp
  deriving stock (Bounded , Show, Read, Eq, Ord, Enum)

data CodelChooser = CCLeft | CCRight
  deriving stock (Show, Read, Eq, Ord, Enum)
