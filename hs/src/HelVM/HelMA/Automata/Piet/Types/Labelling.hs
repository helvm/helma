
module HelVM.HelMA.Automata.Piet.Types.Labelling
  ( Labelling (..)
  , info
  , mask
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Label

import           Lens.Micro.Platform

data Labelling
  = Labelling
      { _mask :: Image LabelKey
      , _info :: IntMap (Maybe LabelInfo)
      }
  deriving stock (Show)

makeLenses ''Labelling
