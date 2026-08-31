module HelVM.HelMA.Automata.Piet.Types.Course
  ( Course (..)
  , codelChooserL
  , directionPointerL
  , furthest
  , initialCourse
  , rotateDirectionPointer
  , rotateToggle
  , toggleCodelChooser
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

import           Relude.Extra

-- TYPES & LENSES

data Course
  = Course
      { directionPointer :: !DirectionPointer
      , codelChooser     :: !CodelChooser
      }
  deriving stock (Eq, Show)

-- Ręczne definicje lense'ów zastępujące `makeLenses ''Course`

directionPointerL ∷ Lens' Course DirectionPointer
directionPointerL = lens directionPointer (\s x -> s { directionPointer = x })

codelChooserL ∷ Lens' Course CodelChooser
codelChooserL = lens codelChooser (\s x -> s { codelChooser = x })

-- FUNCTIONS

furthest ∷ Course → Coordinates → Coordinates → Ordering
furthest (Course DPLeft CCLeft)   = flip (comparing fst) <> comparing snd
furthest (Course DPRight CCLeft)  = comparing fst <> flip (comparing snd)
furthest (Course DPUp CCLeft)     = flip (comparing snd <> comparing fst)
furthest (Course DPDown CCLeft)   = comparing snd <> comparing fst
furthest (Course DPLeft CCRight)  = flip (comparing fst <> comparing snd)
furthest (Course DPRight CCRight) = comparing fst <> comparing snd
furthest (Course DPUp CCRight)    = flip (comparing snd) <> comparing fst
furthest (Course DPDown CCRight)  = comparing snd <> flip (comparing fst)

rotateDirectionPointer ∷ Int → Course → Course
rotateDirectionPointer n = directionPointerL %~ rotate n

toggleCodelChooser ∷ Int → Course → Course
toggleCodelChooser n = codelChooserL %~ toggle n

rotateToggle ∷ Coordinates → Course → Course
rotateToggle (r, t) = rotateDirectionPointer r . toggleCodelChooser t

initialCourse ∷ Course
initialCourse = Course DPRight CCLeft
