module HelVM.HelMA.Automata.Piet.Types.ProgramConfig
  ( CodelSize
  , ProgramConfig (..)
  , codelSize
  , colorMap
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image

import           Lens.Micro.Platform

type CodelSize = Int

data ProgramConfig
  = ProgramConfig
      { _codelSize :: CodelSize
      , _colorMap  :: Image Color
      }
  deriving stock (Show)

makeLenses ''ProgramConfig
