module HelVM.HelMA.Automata.Piet.API.ImageConfig
  ( AdditionalColorStrategy (..)
  , CodelSizeMaybe
  , ImageConfig (..)
  , MulticoloredCodelStrategy (..)
  ) where

data ImageConfig
  = ImageConfig
      { additionalColor   :: AdditionalColorStrategy
      , multicoloredCodel :: MulticoloredCodelStrategy
      , codelSize         :: CodelSizeMaybe
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

type CodelSizeMaybe = Maybe Int
