module HelVM.HelMA.Automaton.API.AppOptions where

import qualified HelVM.HelMA.Automaton.API.AutoOptions   as API
import           HelVM.HelMA.Automaton.API.BoolTypes     as API
import           HelVM.HelMA.Automaton.API.Emit          as API
import qualified HelVM.HelMA.Automaton.API.EvalParams    as API
import           HelVM.HelMA.Automaton.API.IOTypes       as API
import qualified HelVM.HelMA.Automaton.API.Lang          as API
import           HelVM.HelMA.Automaton.API.LogLevel      as API
import qualified HelVM.HelMA.Automaton.API.MemoryOptions as API

import           HelVM.HelMA.Automaton.Types.LabelType

defaultAppOptions ∷ AppOptions
defaultAppOptions = AppOptions
  { verbosity     = defaultLogLevel
  , emit          = defaultEmit
  , exec          = False
  , formatType    = BinaryLabel
  , memoryOptions = API.defaultMemoryOptions
  , autoOptions   = API.defaultAutoOptions
  , langCommand   = API.CatCommand
  , file          = ""
  }

-- | Methods

evalParams ∷ AppOptions → Source → API.EvalParams
evalParams o source = API.EvalParams (formatType o) source (memoryOptions o) (autoOptions o)

-- | Types

data AppOptions
  = AppOptions
      { verbosity     :: !LogLevel
      , emit          :: !Emit
      , exec          :: !Exec
      , formatType    :: !LabelType
      , memoryOptions :: !API.MemoryOptions
      , autoOptions   :: !API.AutoOptions
      , langCommand   :: !API.LangCommand
      , file          :: !FilePath
      }
