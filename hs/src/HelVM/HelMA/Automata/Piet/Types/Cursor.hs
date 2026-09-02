module HelVM.HelMA.Automata.Piet.Types.Cursor
  ( Cursor (..)
  , codelChooserIC
  , courseL
  , directionPointerIC
  , initialCursor
  , positionL
  , rotateDirectionPointerIC
  , toggleCodelChooserIC
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

import           Relude.Extra

-- TYPES & LENSES

data Cursor
  = Cursor
      { position :: !Coordinates
      , course   :: !Course
      }
  deriving stock (Eq, Show)

positionL ∷ Lens' Cursor Coordinates
positionL = lens position (\s x -> s { position = x })

courseL ∷ Lens' Cursor Course
courseL = lens course (\s x -> s { course = x })

-- HELPER FUNCTIONS

directionPointerIC ∷ Cursor → DirectionPointer
directionPointerIC ic = ic ^. (courseL . directionPointerL)

codelChooserIC ∷ Cursor → CodelChooser
codelChooserIC ic = ic ^. (courseL . codelChooserL)

rotateDirectionPointerIC ∷ Int → Cursor → Cursor
rotateDirectionPointerIC n = courseL %~ rotateDirectionPointer n

toggleCodelChooserIC ∷ Int → Cursor → Cursor
toggleCodelChooserIC n = courseL %~ toggleCodelChooser n

initialCursor ∷ Cursor
initialCursor = Cursor initialCoordinates initialCourse
