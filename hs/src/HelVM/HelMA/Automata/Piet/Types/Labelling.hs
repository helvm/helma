
module HelVM.HelMA.Automata.Piet.Types.Labelling
  ( Labelling (..)
  , info
  , mask
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           Lens.Micro.Platform

data Labelling
  = Labelling
      { _mask :: Matrix LabelKey
      , _info :: IntMap (Maybe LabelInfo)
      }
  deriving stock (Show)

makeLenses ''Labelling
