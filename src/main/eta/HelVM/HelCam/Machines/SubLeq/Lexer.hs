module HelVM.HelCam.Machines.SubLeq.Lexer where

import HelVM.HelCam.Machines.SubLeq.Symbol
import HelVM.HelCam.Common.Util

import Data.List.Split

tokenize :: Source -> SymbolList
tokenize source = (maybeToList . readMaybe) =<< splitOneOf " \t\n" source
