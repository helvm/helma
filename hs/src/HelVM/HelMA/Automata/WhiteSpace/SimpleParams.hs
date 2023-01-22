module HelVM.HelMA.Automata.WhiteSpace.SimpleParams where

import           HelVM.HelMA.Automaton.API.AutoParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType


-- | Construction for tests
simpleParamsWithDefaultsAndWhiteTokenType :: FormatType -> Source -> SimpleParams
simpleParamsWithDefaultsAndWhiteTokenType = simpleParamsWithDefaults WhiteTokenType

simpleParamsWithDefaultsAndVisibleTokenType :: FormatType -> Source -> SimpleParams
simpleParamsWithDefaultsAndVisibleTokenType = simpleParamsWithDefaults VisibleTokenType

simpleParamsWithDefaults :: TokenType -> FormatType -> Source -> SimpleParams
simpleParamsWithDefaults tt = simpleParams tt (defaultStackType , defaultRAMType)

-- | Construction for benchmark
simpleParamsWithWhiteTokenType :: (StackType, RAMType) -> FormatType -> Source -> SimpleParams
simpleParamsWithWhiteTokenType = simpleParams WhiteTokenType

simpleParamsWithVisibleTokenType :: (StackType, RAMType) -> FormatType -> Source -> SimpleParams
simpleParamsWithVisibleTokenType = simpleParams VisibleTokenType

simpleParams :: TokenType -> (StackType, RAMType) -> FormatType -> Source -> SimpleParams
simpleParams tt (st , rt) al s = SimpleParams
  { tokenType = tt
  , source = s
  , formatType = al
  , stackType = st
  , ramType = rt
  , autoParams = simpleAutoParams False
  }

-- | Type
data SimpleParams = SimpleParams
  { tokenType  :: !TokenType
  , source     :: !Source
  , formatType :: !FormatType
  , stackType  :: !StackType
  , ramType    :: !RAMType
  , autoParams :: !AutoParams
  }


