module HelVM.HelMA.Automata.Piet.Types.Labelling
  ( Labelling (..)
  , infoL
  , maskL
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           Relude.Extra

-- TYPES & LENSES

data Labelling
  = Labelling
      { mask :: Matrix LabelKey
      , info :: IntMap (Maybe LabelInfo)
      }
  deriving stock (Show)

maskL ∷ Lens' Labelling (Matrix LabelKey)
maskL = lens mask (\s x -> s { mask = x })

infoL ∷ Lens' Labelling (IntMap (Maybe LabelInfo))
infoL = lens info (\s x -> s { info = x })
