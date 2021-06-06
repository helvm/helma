module HelVM.HelMA.Common.API.TypeOptions where

import HelVM.HelMA.Common.Types.CellType
import HelVM.HelMA.Common.Types.IntCellType
import HelVM.HelMA.Common.Types.StackType
import HelVM.HelMA.Common.Types.RAMType

data TypeOptions = TypeOptions { ram     :: RAMType
                               , stack   :: StackType
                               , cell    :: CellType
                               , intCell :: IntCellType
                               }
