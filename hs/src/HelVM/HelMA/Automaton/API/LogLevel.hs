module HelVM.HelMA.Automaton.API.LogLevel where

import           Data.Default
import qualified Data.DList   as DList

logToTextLn :: Log -> Text
logToTextLn m = logToText m <> "\n"

logToText :: Log -> Text
logToText (l , m) = show l <> " " <> m

type Logs = DList.DList Log

type Log = (LogLevel , Text)

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
