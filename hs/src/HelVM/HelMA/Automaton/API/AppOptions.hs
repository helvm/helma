module HelVM.HelMA.Automaton.API.AppOptions where

import qualified HelVM.HelMA.Automaton.API.AutoOptions       as API
import           HelVM.HelMA.Automaton.API.BoolTypes         as API
import           HelVM.HelMA.Automaton.API.Emit              as API
import qualified HelVM.HelMA.Automaton.API.EvalParams        as API
import           HelVM.HelMA.Automaton.API.IOTypes           as API
import qualified HelVM.HelMA.Automaton.API.Lang              as API
import           HelVM.HelMA.Automaton.API.LogLevel          as API
import qualified HelVM.HelMA.Automaton.API.MemoryOptions     as API
import           HelVM.HelMA.Automaton.API.OptimizationLevel as API

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.LabelType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Methods

evalParams ∷ AppOptions → Source → API.EvalParams
evalParams o source = API.EvalParams (formatType o) source (memoryOptions o) (autoOptions o)

memoryOptions ∷ AppOptions → API.MemoryOptions
memoryOptions o = API.MemoryOptions (ramType o) (stackType o) (cellType o) (intCellType o)

autoOptions ∷ AppOptions → API.AutoOptions
autoOptions o = API.AutoOptions (API.fromBool $ optimizationFlag o) Nothing (dumpType o)

-- | Types

data AppOptions
  = AppOptions
      { emit             :: !Emit
      , verbosity        :: !LogLevel
      , optimizationFlag :: !Optimization
      , formatType       :: !LabelType
      , ramType          :: !RAMType
      , stackType        :: !StackType
      , cellType         :: !CellType
      , intCellType      :: !IntCellType
      , dumpType         :: !DumpType
      , codelSize        :: !(Maybe Int)
      , exec             :: !Exec
      , langCommand      :: !API.LangCommand
      , file             :: !FilePath
      }

