module HelVM.HelMA.Automata.Piet.PietRequest where

import           HelVM.HelMA.Automata.Piet.Common.LogLevel

data PietRequest
  = Read  PietType
  | Print PietType Int
  | Log LogLevel String
  | Terminate
  deriving stock (Show, Eq, Ord)

data PietType
  = PietNumber
  | PietChar
  deriving stock (Show, Read, Eq, Ord)
