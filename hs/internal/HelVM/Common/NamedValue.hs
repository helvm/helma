module HelVM.Common.NamedValue where

data NamedValue a = NamedValue { name :: !String , value :: !a}
