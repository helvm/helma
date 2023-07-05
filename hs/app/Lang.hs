module Lang where

import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelMA.Automata.BrainFuck.API.BFType
import           HelVM.HelMA.Automata.ETA.API.ETAImplType

data LangWithOptions = LangWithOptions
  { lang        :: !Lang
  , bfType      :: !BFType
  , etaImplType :: !ETAImplType
  , tokenType   :: !TokenType
  }

-- Lang

defaultLang :: Lang
defaultLang = minBound

langs :: NonEmpty Lang
langs = universeNonEmpty

data Lang = Cat | Rev | BF | ETA | F | Lazy | Piet | SQ | WS | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
