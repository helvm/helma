module HelVM.HelMA.Automata.WhiteSpace.SimpleParams where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

-- | Construction for tests
simpleParamsWithDefaultsAndWhiteTokenType :: Bool -> Source -> SimpleParams
simpleParamsWithDefaultsAndWhiteTokenType = simpleParamsWithDefaults WhiteTokenType

simpleParamsWithDefaultsAndVisibleTokenType :: Bool -> Source -> SimpleParams
simpleParamsWithDefaultsAndVisibleTokenType = simpleParamsWithDefaults VisibleTokenType

simpleParamsWithDefaults :: TokenType -> Bool -> Source -> SimpleParams
simpleParamsWithDefaults tt = simpleParams tt (defaultStackType , defaultRAMType)

-- | Construction for benchmark
simpleParamsWithWhiteTokenType :: (StackType, RAMType) -> Bool -> Source -> SimpleParams
simpleParamsWithWhiteTokenType = simpleParams WhiteTokenType

simpleParamsWithVisibleTokenType :: (StackType, RAMType) -> Bool -> Source -> SimpleParams
simpleParamsWithVisibleTokenType = simpleParams VisibleTokenType

simpleParams :: TokenType -> (StackType, RAMType) -> Bool -> Source -> SimpleParams
simpleParams tt (st , rt) al s = SimpleParams
  { tokenType = tt
  , source = s
  , asciiLabel = al
  , stackType = st
  , ramType = rt
  , dumpType = Pretty
  }

-- | Type
data SimpleParams = SimpleParams
  { tokenType  :: !TokenType
  , source     :: !Source
  , asciiLabel :: !Bool
  , stackType  :: !StackType
  , ramType    :: !RAMType
  , dumpType   :: !DumpType
  }


