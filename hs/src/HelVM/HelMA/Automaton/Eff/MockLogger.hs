module HelVM.HelMA.Automaton.Eff.MockLogger (
  filterLogsWithLevelInfo,
  filterLogsWithLevelDebug,
  filterLogsWithLevel,
  mockLog,

  MonadMockLogger,
  MockLogs,
  MockLog (..),
) where

import           HelVM.HelMA.Automaton.API.IOTypes
import qualified HelVM.HelMA.Automaton.API.LogLevel as LogLevel

import           Control.Monad.Logger
import           Control.Monad.Writer.Class         (MonadWriter, tell)

import           Data.MonoTraversable

import qualified Data.Sequence                      as Seq

filterLogsWithLevelInfo :: MockLogs -> Output
filterLogsWithLevelInfo = filterLogsWithLevel LevelInfo

filterLogsWithLevelDebug :: MockLogs -> Output
filterLogsWithLevelDebug = filterLogsWithLevel LevelDebug

filterLogsWithLevel :: LogLevel -> MockLogs -> Output
filterLogsWithLevel t logsSeq = oconcat (line <$> Seq.filter condition logsSeq) where
  condition l = t <= logLevel l
  line l = (LogLevel.showEitherTextLogLevel . LogLevel.fromLogger . logLevel) l <> " " <> (decodeUtf8 . fromLogStr . logStr) l <> "\n"

mockLog :: MonadMockLogger m => MockLog -> m ()
mockLog = tell . one

----

type MonadMockLogger m = MonadWriter MockLogs m

type MockLogs = Seq MockLog

data MockLog = MockLog
  { logLoc    :: !Loc
  , logSource :: !LogSource
  , logLevel  :: !LogLevel
  , logStr    :: !LogStr
  }
  deriving stock (Eq, Show)
