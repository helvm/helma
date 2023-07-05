module HelVM.HelMA.Automata.Piet.LogLevel where

import qualified Text.Show

data LogLevel = Verbosed | Info | Error | Fatal
  deriving stock (Eq, Ord)

instance Show LogLevel where
  show Verbosed = "VERBOSED"
  show Info     = "INFO    "
  show Error    = "ERROR   "
  show Fatal    = "FATAL   "
