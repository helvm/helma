module HelVM.HelMA.Automata.Piet.Types.Program
  ( Program (..)
  , codelSizeL
  , imageL
  , isBlocked
  , labellingL
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.Labelling

import           HelVM.HelMA.Automata.Piet.API.CodelSize

import           Relude.Extra

-- TYPES & LENSES

data Program
  = Program
      { codelSize :: CodelSizeInternal
      , image     :: Grid Color
      , labelling :: Labelling
      }
  deriving stock (Show)

codelSizeL ∷ Lens' Program CodelSizeInternal
codelSizeL = lens codelSize (\s x -> s { codelSize = x })

imageL ∷ Lens' Program (Grid Color)
imageL = lens image (\s x -> s { image = x })

labellingL ∷ Lens' Program Labelling
labellingL = lens labelling (\s x -> s { labelling = x })

-- HELPER FUNCTIONS

isBlocked ∷ Coordinates → Program → Bool
isBlocked pos p = not (inRangeGrid pos $ p ^. imageL) || (Black == atGrid pos (p ^. imageL))
