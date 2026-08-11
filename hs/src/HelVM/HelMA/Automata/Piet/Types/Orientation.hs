module HelVM.HelMA.Automata.Piet.Types.Orientation
  ( Orientation (..)
  , initialOrientation
  , rotateDirectionPointer
  , rotateToggle
  , toggleCodelChooser
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

rotateDirectionPointer ∷ Int → Orientation → Orientation
rotateDirectionPointer n o = o { directionPointer = rotate n (directionPointer o)}

toggleCodelChooser ∷ Int → Orientation → Orientation
toggleCodelChooser n o = o { codelChooser = toggle n (codelChooser o)}

rotateToggle ∷ Coordinates → Orientation → Orientation
rotateToggle (r, t) (Orientation dp cc) = Orientation (rotate r dp) (toggle t cc)

initialOrientation ∷ Orientation
initialOrientation = Orientation DPRight CCLeft

data Orientation
  = Orientation
      { directionPointer :: !DirectionPointer
      , codelChooser     :: !CodelChooser
      }
