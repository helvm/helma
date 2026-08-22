module HelVM.HelMA.Automata.Piet.Types.Orientation
  ( Orientation (..)
  , furthest
  , initialOrientation
  , rotateDirectionPointer
  , rotateToggle
  , toggleCodelChooser
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

furthest ∷ CodelChooser → DirectionPointer → Coordinates → Coordinates → Ordering
furthest CCLeft DPLeft   = flip (comparing fst) <> comparing snd
furthest CCLeft DPRight  = comparing fst <> flip (comparing snd)
furthest CCLeft DPUp     = flip (comparing snd <> comparing fst)
furthest CCLeft DPDown   = comparing snd <> comparing fst
furthest CCRight DPLeft  = flip (comparing fst <> comparing snd)
furthest CCRight DPRight = comparing fst <> comparing snd
furthest CCRight DPUp    = flip (comparing snd) <> comparing fst
furthest CCRight DPDown  = comparing snd <> flip (comparing fst)

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
