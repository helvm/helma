module HelVM.HelCam.Machines.SubLeq.Lexer where

import HelVM.HelCam.Common.Util

import Data.List.Split

tokenize :: Source -> [Int]
tokenize source = (maybeToList . readMaybe) =<< splitOneOf " \t\n" source
