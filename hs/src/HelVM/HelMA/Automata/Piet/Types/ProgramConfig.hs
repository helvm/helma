module HelVM.HelMA.Automata.Piet.Types.ProgramConfig
  ( CodelSize
  , ProgramConfig (..)
  , codelSize
  , image
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image

import           Lens.Micro.Platform

type CodelSize = Int

data ProgramConfig
  = ProgramConfig
      { _codelSize :: CodelSize
      , _image     :: Image Color
      }
  deriving stock (Show)

makeLenses ''ProgramConfig
