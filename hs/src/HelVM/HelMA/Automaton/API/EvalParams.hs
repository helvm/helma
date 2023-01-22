module HelVM.HelMA.Automaton.API.EvalParams where

import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.AutoParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Accessors
ramAutoOptions :: EvalParams -> RAMType
ramAutoOptions = ram . typeOptions

stackAutoOptions :: EvalParams -> StackType
stackAutoOptions = stack . typeOptions

cellAutoOptions :: EvalParams -> CellType
cellAutoOptions = cell . typeOptions

intCellAutoOptions :: EvalParams -> IntCellType
intCellAutoOptions = intCell . typeOptions

dumpAutoOptions :: EvalParams -> DumpType
dumpAutoOptions = dumpType . autoParams

-- | Type
data EvalParams = EvalParams
  { formatType  :: !FormatType
  , source      :: !Source
  , typeOptions :: !AutoOptions
  , autoParams  :: !AutoParams
  }
