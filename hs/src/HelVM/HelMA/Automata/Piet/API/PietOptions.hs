module HelVM.HelMA.Automata.Piet.API.PietOptions where

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.ImplType                 
import           HelVM.HelMA.Automata.Piet.API.LexerType
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

data PietOptions = PietOptions
  { implType                  :: !ImplType
  , additionalColorStrategy   :: !(Maybe AdditionalColorStrategy)
  , multicoloredCodelStrategy :: !(Maybe MulticoloredCodelStrategy)
  , limit                     :: !(Maybe Natural)
  , lexerType                 :: !(Maybe LexerType)
  } deriving stock (Eq, Show)