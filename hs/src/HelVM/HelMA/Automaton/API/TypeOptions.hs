module HelVM.HelMA.Automaton.API.TypeOptions where

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.IntCellType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

data TypeOptions = TypeOptions
  { ram     :: !RAMType
  , stack   :: !StackType
  , cell    :: !CellType
  , intCell :: !IntCellType
  }
