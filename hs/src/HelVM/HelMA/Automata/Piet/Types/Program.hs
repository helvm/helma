module HelVM.HelMA.Automata.Piet.Types.Program
  ( CodelSize
  , Program (..)
  , codelSize
  , image
  , isBlocked
  , labelling
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Labelling
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           Relude.Extra

-- TYPES & LENSES

type CodelSize = Int

data Program
  = Program
      { _codelSize :: CodelSize
      , _image     :: Matrix Color
      , _labelling :: Labelling
      }
  deriving stock (Show)

codelSize ∷ Lens' Program CodelSize
codelSize = lens _codelSize (\s x -> s { _codelSize = x })

image ∷ Lens' Program (Matrix Color)
image = lens _image (\s x -> s { _image = x })

labelling ∷ Lens' Program Labelling
labelling = lens _labelling (\s x -> s { _labelling = x })

-- HELPER FUNCTIONS

isBlocked ∷ Coordinates → Program → Bool
isBlocked pos p = not (inRangeMatrix pos $ p ^. image) || (Black == atMatrix pos (p ^. image))
