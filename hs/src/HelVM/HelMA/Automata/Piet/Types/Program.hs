module HelVM.HelMA.Automata.Piet.Types.Program
  ( Program (..)
  , image
  , isBlocked
  , labelling
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Labelling

import           Lens.Micro                                  ( (^.) )
import           Lens.Micro.TH                               ( makeLenses )

data Program
  = Program
      { _image     :: Image Color
      , _labelling :: Labelling
      }
  deriving stock (Show)

makeLenses ''Program

isBlocked ∷ Coordinates → Program → Bool
isBlocked pos p = not (inRangeImage pos $ p ^. image) || (Black == pixelImage pos (p ^. image))
