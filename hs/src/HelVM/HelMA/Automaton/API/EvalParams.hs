module HelVM.HelMA.Automaton.API.EvalParams where

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.API.TypeOptions

data EvalParams = EvalParams { asciiLabel :: Bool
                             , source :: Source
                             , typeOptions :: TypeOptions
                             }
