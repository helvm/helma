module HelVM.HelMA.Automaton.API.AppOptions where

import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.EvalOptions
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.LogLevel

import           HelVM.HelMA.LangCommand

-- | Methods
evalParams ∷ AppOptions → Source → EvalParams
evalParams o s = EvalParams s (o.evalOptions)

-- | Types
data AppOptions
  = AppOptions
      { verbosity   :: !LogLevel
      , emit        :: !Emit
      , exec        :: !Exec
      , evalOptions :: !EvalOptions
      , langCommand :: !LangCommand
      , file        :: !FilePath
      }
