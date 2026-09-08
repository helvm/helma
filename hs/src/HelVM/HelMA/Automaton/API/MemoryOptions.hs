module HelVM.HelMA.Automaton.API.MemoryOptions where

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

simpleMemoryOptions ∷ MemoryOptions
simpleMemoryOptions = MemoryOptions
  defaultRAMType
  defaultStackType
  defaultCellType
  defaultIntCellType

-- | Types
data MemoryOptions
  = MemoryOptions
      { ram     :: !RAMType
      , stack   :: !StackType
      , cell    :: !CellType
      , intCell :: !IntCellType
      }
