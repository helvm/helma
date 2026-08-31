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

import           Lens.Micro.Platform

type CodelSize = Int

data Program
  = Program
      { _codelSize :: CodelSize
      , _image     :: Matrix Color
      , _labelling :: Labelling
      }
  deriving stock (Show)

makeLenses ''Program

isBlocked ∷ Coordinates → Program → Bool
isBlocked pos p = not (inRangeMatrix pos $ p ^. image) || (Black == atMatrix pos (p ^. image))
