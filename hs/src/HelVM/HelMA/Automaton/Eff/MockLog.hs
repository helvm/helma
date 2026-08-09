module HelVM.HelMA.Automaton.Eff.MockLog (
  calculateLogsWithLevelInfo,
  calculateLogsWithLevelDebug,
  calculateLogsWithLevel,

  MockLogs,
  MockLog (..),
) where

import           HelVM.HelMA.Automaton.API.IOTypes
import qualified HelVM.HelMA.Automaton.API.LogLevel as LogLevel

import           Control.Monad.Logger

import           Data.MonoTraversable

import qualified Data.Sequence                      as Seq

calculateLogsWithLevelInfo :: MockLogs -> Output
calculateLogsWithLevelInfo = calculateLogsWithLevel LevelInfo

calculateLogsWithLevelDebug :: MockLogs -> Output
calculateLogsWithLevelDebug = calculateLogsWithLevel LevelDebug

calculateLogsWithLevel :: LogLevel -> MockLogs -> Output
calculateLogsWithLevel t logsSeq = oconcat (line <$> Seq.filter condition logsSeq) where
  condition l = t <= logLevel l
  line l = (LogLevel.showEitherTextLogLevel . LogLevel.fromLogger . logLevel) l <> " " <> (decodeUtf8 . fromLogStr . logStr) l <> "\n"

type MockLogs = Seq MockLog

data MockLog = MockLog
  { logLoc    :: !Loc
  , logSource :: !LogSource
  , logLevel  :: !LogLevel
  , logStr    :: !LogStr
  }
  deriving stock (Eq, Show)
