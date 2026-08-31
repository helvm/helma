module HelVM.HelMA.Automata.Piet.Types.Labelling
  ( Labelling (..)
  , info
  , mask
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           Relude.Extra

-- TYPES & LENSES

data Labelling
  = Labelling
      { _mask :: Matrix LabelKey
      , _info :: IntMap (Maybe LabelInfo)
      }
  deriving stock (Show)

mask ∷ Lens' Labelling (Matrix LabelKey)
mask = lens _mask (\s x -> s { _mask = x })

info ∷ Lens' Labelling (IntMap (Maybe LabelInfo))
info = lens _info (\s x -> s { _info = x })
