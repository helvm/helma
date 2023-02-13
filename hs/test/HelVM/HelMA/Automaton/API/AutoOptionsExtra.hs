module HelVM.HelMA.Automaton.API.AutoOptionsExtra where

import           HelVM.HelMA.Automaton.API.MemoryOptions

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

defaultAutoOptions :: MemoryOptions
defaultAutoOptions = MemoryOptions
  { ram      = defaultRAMType
  , stack    = defaultStackType
  , cell     = defaultCellType
  , intCell  = defaultIntCellType
  }
