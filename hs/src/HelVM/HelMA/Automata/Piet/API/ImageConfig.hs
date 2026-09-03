module HelVM.HelMA.Automata.Piet.API.ImageConfig
  ( CodelSize
  , ImageConfig (..)
  ) where

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

data ImageConfig
  = ImageConfig
      { additionalColor   :: AdditionalColorStrategy
      , multicoloredCodel :: MulticoloredCodelStrategy
      , codelSize         :: Maybe CodelSize
      }
  deriving stock (Eq, Show)

type CodelSize = Int
