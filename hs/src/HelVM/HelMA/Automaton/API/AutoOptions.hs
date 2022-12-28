module HelVM.HelMA.Automaton.API.AutoOptions where

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

-- | Types
data AutoOptions = AutoOptions
  { ram      :: !RAMType
  , stack    :: !StackType
  , cell     :: !CellType
  , intCell  :: !IntCellType
  , dumpType :: !DumpType
  }
