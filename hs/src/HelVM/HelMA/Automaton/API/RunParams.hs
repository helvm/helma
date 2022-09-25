module HelVM.HelMA.Automaton.API.RunParams where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.TypeOptions

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Accessors
ramTypeOptions :: RunParams -> RAMType
ramTypeOptions = ram . typeOptions

stackTypeOptions :: RunParams -> StackType
stackTypeOptions = stack . typeOptions

cellTypeOptions :: RunParams -> CellType
cellTypeOptions = cell . typeOptions

intCellTypeOptions :: RunParams -> IntCellType
intCellTypeOptions = intCell . typeOptions

dumpTypeOptions :: RunParams -> DumpType
dumpTypeOptions = dumpType . typeOptions

-- | Type
data RunParams = RunParams
  { compile     :: !Bool
  , formatType  :: !FormatType
  , source      :: !Source
  , typeOptions :: !TypeOptions
  }
