module HelVM.HelMA.Automata.Piet.Types.Orientation
  ( Orientation (..)
  , codelChooser
  , directionPointer
  , furthest
  , initialOrientation
  , rotateDirectionPointer
  , rotateToggle
  , toggleCodelChooser
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

import           Relude.Extra

-- TYPES & LENSES

data Orientation
  = Orientation
      { _directionPointer :: !DirectionPointer
      , _codelChooser     :: !CodelChooser
      }
  deriving stock (Eq, Show)

-- Ręczne definicje lense'ów zastępujące `makeLenses ''Orientation`

directionPointer ∷ Lens' Orientation DirectionPointer
directionPointer = lens _directionPointer (\s x -> s { _directionPointer = x })

codelChooser ∷ Lens' Orientation CodelChooser
codelChooser = lens _codelChooser (\s x -> s { _codelChooser = x })

-- FUNCTIONS

furthest ∷ Orientation → Coordinates → Coordinates → Ordering
furthest (Orientation DPLeft CCLeft)   = flip (comparing fst) <> comparing snd
furthest (Orientation DPRight CCLeft)  = comparing fst <> flip (comparing snd)
furthest (Orientation DPUp CCLeft)     = flip (comparing snd <> comparing fst)
furthest (Orientation DPDown CCLeft)   = comparing snd <> comparing fst
furthest (Orientation DPLeft CCRight)  = flip (comparing fst <> comparing snd)
furthest (Orientation DPRight CCRight) = comparing fst <> comparing snd
furthest (Orientation DPUp CCRight)    = flip (comparing snd) <> comparing fst
furthest (Orientation DPDown CCRight)  = comparing snd <> flip (comparing fst)

rotateDirectionPointer ∷ Int → Orientation → Orientation
rotateDirectionPointer n = directionPointer %~ rotate n

toggleCodelChooser ∷ Int → Orientation → Orientation
toggleCodelChooser n = codelChooser %~ toggle n

rotateToggle ∷ Coordinates → Orientation → Orientation
rotateToggle (r, t) = rotateDirectionPointer r . toggleCodelChooser t

initialOrientation ∷ Orientation
initialOrientation = Orientation DPRight CCLeft
