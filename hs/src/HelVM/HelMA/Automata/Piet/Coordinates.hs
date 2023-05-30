module HelVM.HelMA.Automata.Piet.Coordinates (
  addCoordinates,
) where

addCoordinates :: DirectionPointer -> Coordinates -> Coordinates
addCoordinates DPRight (x , y)  = (x + 1, y)
addCoordinates DPDown   (x , y) =   (x, y + 1)
addCoordinates DPLeft   (x , y) =   (x - 1, y)
addCoordinates DPUp     (x , y) = (x, y - 1)

type Coordinates = (Int , Int)

data DirectionPointer = DPRight | DPDown | DPLeft  | DPUp
  deriving stock (Show, Read, Eq, Ord, Enum)
