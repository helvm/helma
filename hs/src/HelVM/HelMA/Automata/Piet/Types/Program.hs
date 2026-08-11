module HelVM.HelMA.Automata.Piet.Types.Program (
  isBlocked,
  Program(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Label

isBlocked ∷ Coordinates → Program → Bool
isBlocked pos p = not (inRangeImage pos $ image p) || (Black == pixelImage pos (image p))

data Program = Program
  { image :: Image Color
  , mask  :: Image LabelKey
  , info  :: IntMap (Maybe LabelInfo)
  }
