module HelVM.HelMA.Automata.Piet.Types.ProgramConfig
  ( CodelSize
  , ProgramConfig (..)
  , codelSize
  , colorMap
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ColorMap

import           Lens.Micro.TH

type CodelSize = Int

data ProgramConfig
  = ProgramConfig
      { _codelSize :: CodelSize
      , _colorMap  :: ColorMap
      }
  deriving stock (Eq, Show)

makeLenses ''ProgramConfig
