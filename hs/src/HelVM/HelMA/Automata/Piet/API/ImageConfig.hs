module HelVM.HelMA.Automata.Piet.API.ImageConfig
  ( AdditionalColorStrategy (..)
  , CodelSize
  , ImageConfig (..)
  , MulticoloredCodelStrategy (..)
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
