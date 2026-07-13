module HelVM.HelMA.Automaton.API.Lang where

import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelMA.Automata.BrainFuck.API.BFType
import           HelVM.HelMA.Automata.ETA.API.ETAImplType


data LangWithOptions = LangWithOptions
  { lang        :: !Lang
  , bfType      :: !BFType
  , etaImplType :: !ETAImplType
  , tokenType   :: !TokenType
  }

data LangCommand
  = BFCommand !BFType
  | ETACommand !ETAImplType
  | FCommand
  | PietCommand
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
