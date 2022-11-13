module Lang where

import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelMA.Automata.BrainFuck.API.BFType
import           HelVM.HelMA.Automata.ETA.API.ETAImplType

import           HelVM.HelIO.SwitchEnum

data LangWithOptions = LangWithOptions
  { lang        :: !Lang
  , bfType      :: !BFType
  , etaImplType :: !ETAImplType
  , tokenType   :: !TokenType
  }

-- Lang

defaultLang :: Lang
defaultLang = defaultEnum

langs :: [Lang]
langs = generateEnums 9

data Lang = Cat | Rev | BF | ETA | F | Lazy | SQ | WS | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
