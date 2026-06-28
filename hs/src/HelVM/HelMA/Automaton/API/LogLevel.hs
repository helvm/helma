module HelVM.HelMA.Automaton.API.LogLevel where

import           Data.Default

defaultLogLevel :: LogLevel
defaultLogLevel = def

logLevels :: NonEmpty LogLevel
logLevels = universeNonEmpty

data LogLevel =
    Verbosed
  | Info
  | Error
  | Fatal
  deriving stock (Bounded , Enum , Eq , Ord, Read , Show)

instance Default LogLevel where
  def = minBound
