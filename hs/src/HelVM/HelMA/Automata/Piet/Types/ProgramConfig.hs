{-# LANGUAGE TemplateHaskell #-}
module HelVM.HelMA.Automata.Piet.Types.ProgramConfig
  ( CodelSize
  , ProgramConfig (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ColorMap

import           Lens.Micro.TH

type CodelSize = Int

data ProgramConfig
  = ProgramConfig
      { codelSize :: CodelSize
      , colorMap  :: ColorMap
      }
  deriving stock (Eq, Show)

makeLenses ''ProgramConfig
