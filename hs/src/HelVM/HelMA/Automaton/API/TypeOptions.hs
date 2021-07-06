module HelVM.HelMA.Automaton.API.TypeOptions where

import HelVM.HelMA.Automaton.Types.CellType
import HelVM.HelMA.Automaton.Types.IntCellType
import HelVM.HelMA.Automaton.Types.StackType
import HelVM.HelMA.Automaton.Types.RAMType

data TypeOptions = TypeOptions { ram     :: !RAMType
                               , stack   :: !StackType
                               , cell    :: !CellType
                               , intCell :: !IntCellType
                               }
