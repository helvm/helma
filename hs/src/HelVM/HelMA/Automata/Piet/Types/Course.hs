module HelVM.HelMA.Automata.Piet.Types.Course
  ( Course (..)
  , codelChooserL
  , directionPointerL
  , furthest
  , initialCourse
  , rotateDirectionPointer
  , rotateToggle
  , showCourse
  , toggleCodelChooser
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

import           Relude.Extra

-- TYPES & CONSTRUCTORS

data Course
  = Course
      { directionPointer :: !DirectionPointer
      , codelChooser     :: !CodelChooser
      }
  deriving stock (Eq, Ord, Show)

initialCourse ∷ Course
initialCourse = Course DPRight CCLeft

-- LENSES

directionPointerL ∷ Lens' Course DirectionPointer
directionPointerL = lens directionPointer (\s x -> s { directionPointer = x })

codelChooserL ∷ Lens' Course CodelChooser
codelChooserL = lens codelChooser (\s x -> s { codelChooser = x })

-- TRANSFORMATIONS

rotateDirectionPointer ∷ Int → Course → Course
rotateDirectionPointer n = directionPointerL %~ rotate n

toggleCodelChooser ∷ Int → Course → Course
toggleCodelChooser n = codelChooserL %~ toggle n

rotateToggle ∷ Coordinates → Course → Course
rotateToggle (r, t) = rotateDirectionPointer r . toggleCodelChooser t

-- QUERYING / LOGIC

furthest ∷ Course → Coordinates → Coordinates → Ordering
furthest (Course DPLeft CCLeft)   = flip (comparing fst) <> comparing snd
furthest (Course DPRight CCLeft)  = comparing fst <> flip (comparing snd)
furthest (Course DPUp CCLeft)     = flip (comparing snd <> comparing fst)
furthest (Course DPDown CCLeft)   = comparing snd <> comparing fst
furthest (Course DPLeft CCRight)  = flip (comparing fst <> comparing snd)
furthest (Course DPRight CCRight) = comparing fst <> comparing snd
furthest (Course DPUp CCRight)    = flip (comparing snd) <> comparing fst
furthest (Course DPDown CCRight)  = comparing snd <> flip (comparing fst)

-- DISPLAY

showCourse ∷ Course → String
showCourse (Course dp cc) = [charDP dp, charCC cc]
