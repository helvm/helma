module EvalOptions where

import HelVM.HelCam.Common.Types.CellType
import HelVM.HelCam.Common.Types.StackType
import HelVM.HelCam.Common.Types.RAMType  
  
data EvalOptions = EvalOptions { ram :: RAMType
                               , stack :: StackType
                               , cell :: CellType
                               }
