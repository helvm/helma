module HelVM.HelMA.Automaton.API.TypeOptionsExtra where

import           HelVM.HelMA.Automaton.API.TypeOptions

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

defaultTypeOptions :: TypeOptions
defaultTypeOptions = TypeOptions
  { ram      = defaultRAMType
  , stack    = defaultStackType
  , cell     = defaultCellType
  , intCell  = defaultIntCellType
  , dumpType = defaultDumpType
  }
