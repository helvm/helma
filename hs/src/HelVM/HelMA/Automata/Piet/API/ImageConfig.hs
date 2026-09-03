module HelVM.HelMA.Automata.Piet.API.ImageConfig
  ( CodelSize
  , ImageConfig (..)
  , defaultConfig
  ) where

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.CodelSize
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

defaultConfig ∷ ImageConfig
defaultConfig = ImageConfig defaultAdditionalColorStrategy defaultMulticoloredCodelStrategy Nothing

data ImageConfig
  = ImageConfig
      { additionalColor   :: AdditionalColorStrategy
      , multicoloredCodel :: MulticoloredCodelStrategy
      , codelSize         :: Maybe CodelSize
      }
  deriving stock (Eq, Show)
