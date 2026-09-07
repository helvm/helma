module HelVM.HelMA.Automata.Piet.API.PietOptions where

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.CodelSize
import           HelVM.HelMA.Automata.Piet.API.ImageConfig
import           HelVM.HelMA.Automata.Piet.API.ImplType
import           HelVM.HelMA.Automata.Piet.API.LexerType
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

import           HelVM.HelMA.Automaton.API.AutomatonType

imageConfig ∷ PietOptions → ImageConfig
imageConfig po = ImageConfig
  (fromMaybe defaultAdditionalColorStrategy po.additionalColor)
  (fromMaybe defaultMulticoloredCodelStrategy po.multicoloredCodel)
  po.codelSize

data PietOptions
  = PietOptions
      { automatonType     :: !(Maybe AutomatonType)
      , implType          :: !ImplType
      , additionalColor   :: !(Maybe AdditionalColorStrategy)
      , multicoloredCodel :: !(Maybe MulticoloredCodelStrategy)
      , codelSize         :: !(Maybe CodelSize)
      , lexerType         :: !(Maybe LexerType)
      }
  deriving stock (Eq, Show)
