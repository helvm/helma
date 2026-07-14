module HelVM.HelMA.Automaton.API.Lang where

import           HelVM.HelMA.Automata.BrainFuck.API.ImplType
import           HelVM.HelMA.Automata.ETA.API.AutomatonType
import           HelVM.HelMA.Automata.Piet.API.LexerType
import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType

data LangCommand
  = BFCommand !ImplType
  | ETACommand !AutomatonType
  | FCommand
  | PietCommand !(Maybe Natural) !(Maybe LexerType)
  | SQCommand
  | WSCommand !TokenType
  | CatCommand
  | RevCommand
  | LazyCommand
  | ZotCommand

-- Lang

defaultLang :: Lang
defaultLang = minBound

langs :: NonEmpty Lang
langs = universeNonEmpty

data Lang = BF | ETA | F | Piet | SQ | WS | Cat | Rev | Lazy | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
