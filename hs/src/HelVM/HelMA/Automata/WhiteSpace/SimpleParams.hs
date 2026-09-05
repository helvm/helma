module HelVM.HelMA.Automata.WhiteSpace.SimpleParams where

import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType

import qualified HelVM.HelMA.Automaton.API.AutomatonOptions    as Automaton
import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Types.LabelType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Construction for tests
simpleParamsWithDefaultsAndWhiteTokenType ∷ LabelType → Source → SimpleParams
simpleParamsWithDefaultsAndWhiteTokenType = simpleParamsWithDefaults WhiteTokenType

simpleParamsWithDefaultsAndVisibleTokenType ∷ LabelType → Source → SimpleParams
simpleParamsWithDefaultsAndVisibleTokenType = simpleParamsWithDefaults VisibleTokenType

simpleParamsWithDefaults ∷ TokenType → LabelType → Source → SimpleParams
simpleParamsWithDefaults tt = simpleParams tt (defaultStackType , defaultRAMType)

-- | Construction for benchmark
simpleParamsWithWhiteTokenType ∷ (StackType, RAMType) → LabelType → Source → SimpleParams
simpleParamsWithWhiteTokenType = simpleParams WhiteTokenType

simpleParamsWithVisibleTokenType ∷ (StackType, RAMType) → LabelType → Source → SimpleParams
simpleParamsWithVisibleTokenType = simpleParams VisibleTokenType

automatonOptions ∷ SimpleParams → Automaton.AutomatonOptions
automatonOptions p = Automaton.AutomatonOptions (stackType p) (ramType p) (autoOptions p)

simpleParams ∷ TokenType → (StackType, RAMType) → LabelType → Source → SimpleParams
simpleParams tt (st , rt) al s = SimpleParams
  { tokenType = tt
  , source = s
  , labelType = al
  , stackType = st
  , ramType = rt
  , autoOptions = simpleAutoParams
  }

-- | Type
data SimpleParams
  = SimpleParams
      { tokenType   :: !TokenType
      , source      :: !Source
      , labelType   :: !LabelType
      , stackType   :: !StackType
      , ramType     :: !RAMType
      , autoOptions :: !AutoOptions
      }
