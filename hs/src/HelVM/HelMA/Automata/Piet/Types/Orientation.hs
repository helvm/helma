module HelVM.HelMA.Automata.Piet.Types.Orientation (
  rotateToggle,
  initialOrientation,
  Orientation(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

rotateToggle :: Orientation -> Coordinates -> Orientation
rotateToggle (Orientation dp cc) (r, t) = Orientation (rotate r dp) (toggle t cc)

initialOrientation :: Orientation
initialOrientation = Orientation DPRight CCLeft

data Orientation = Orientation
  { directionPointer :: !DirectionPointer
  , codelChooser     :: !CodelChooser
  }
