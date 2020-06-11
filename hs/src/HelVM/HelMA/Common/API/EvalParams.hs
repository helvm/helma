module HelVM.HelMA.Common.API.EvalParams where

import HelVM.HelMA.Common.API.TypeOptions
import HelVM.HelMA.Common.Util

data EvalParams = EvalParams { asciiLabel :: Bool
                             , source :: Source
                             , typeOptions :: TypeOptions
                             }
