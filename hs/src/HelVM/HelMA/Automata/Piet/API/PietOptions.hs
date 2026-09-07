module HelVM.HelMA.Automata.Piet.API.PietOptions where

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.ImageConfig
import           HelVM.HelMA.Automata.Piet.API.ImplType
import           HelVM.HelMA.Automata.Piet.API.LexerType
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

imageConfig ∷ PietOptions → ImageConfig
imageConfig po = ImageConfig (fromMaybe defaultAdditionalColorStrategy po.additionalColor) (fromMaybe defaultMulticoloredCodelStrategy po.multicoloredCodel) po.limit

data PietOptions
  = PietOptions
      { implType          :: !ImplType
      , additionalColor   :: !(Maybe AdditionalColorStrategy)
      , multicoloredCodel :: !(Maybe MulticoloredCodelStrategy)
      , limit             :: !(Maybe Natural)
      , lexerType         :: !(Maybe LexerType)
      }
  deriving stock (Eq, Show)
