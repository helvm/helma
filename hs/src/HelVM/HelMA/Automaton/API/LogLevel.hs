module HelVM.HelMA.Automaton.API.LogLevel where

import           Data.Default

logToTextLn :: (LogLevel , Text) -> Text
logToTextLn m = logToText m <> "\n"

logToText :: (LogLevel , Text) -> Text
logToText (l , m) = show l <> " " <> m

defaultLogLevel :: LogLevel
defaultLogLevel = def

logLevels :: NonEmpty LogLevel
logLevels = universeNonEmpty

data LogLevel =
    Error
  | Warn
  | Info
  | Debug
  deriving stock (Bounded , Enum , Eq , Ord, Read , Show)

instance Default LogLevel where
  def = minBound
