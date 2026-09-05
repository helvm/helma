module HelVM.HelMA.Automaton.API.AppOptions where

import qualified HelVM.HelMA.Automaton.API.AutoOptions   as API
import           HelVM.HelMA.Automaton.API.BoolTypes     as API
import           HelVM.HelMA.Automaton.API.Emit          as API
import qualified HelVM.HelMA.Automaton.API.EvalParams    as API
import           HelVM.HelMA.Automaton.API.IOTypes       as API
import qualified HelVM.HelMA.Automaton.API.Lang          as API
import           HelVM.HelMA.Automaton.API.LogLevel      as API
import qualified HelVM.HelMA.Automaton.API.MemoryOptions as API

import           HelVM.HelMA.Automaton.API.LabelType

-- | Methods

evalParams ∷ AppOptions → Source → API.EvalParams
evalParams o source = API.EvalParams (labelType o) source (memoryOptions o) (autoOptions o)

-- | Types

data AppOptions
  = AppOptions
      { verbosity     :: !LogLevel
      , emit          :: !Emit
      , exec          :: !Exec
      , labelType     :: !LabelType
      , memoryOptions :: !API.MemoryOptions
      , autoOptions   :: !API.AutoOptions
      , langCommand   :: !API.LangCommand
      , file          :: !FilePath
      }

