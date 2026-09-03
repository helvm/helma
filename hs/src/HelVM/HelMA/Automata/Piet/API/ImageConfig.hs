module HelVM.HelMA.Automata.Piet.API.ImageConfig
  ( AdditionalColorStrategy (..)
  , CodelSize
  , ImageConfig (..)
  , MulticoloredCodelStrategy (..)
  ) where

data ImageConfig
  = ImageConfig
      { additionalColor   :: AdditionalColorStrategy
      , multicoloredCodel :: MulticoloredCodelStrategy
      , codelSize         :: Maybe CodelSize
      }
  deriving stock (Eq, Show)

data AdditionalColorStrategy
  = AdditionalColorAsWhite
  | AdditionalColorAsBlack
  | AdditionalColorNearest
  deriving stock (Eq, Ord, Show)

data MulticoloredCodelStrategy
  = MulticoloredCodelAsWhite
  | MulticoloredCodelAsBlack
  | MulticoloredCodelCenter
  | MulticoloredCodelModal
  | MulticoloredCodelAverage
  deriving stock (Eq, Ord, Show)

type CodelSize = Int
