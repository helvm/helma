module HelVM.HelMA.Automaton.Eff.MockLogger
  ( MockLog (..)
  , MockLoggerData
  , MonadMockLogger
  , filterLogsWithLevel
  , filterLogsWithLevelDebug
  , filterLogsWithLevelInfo
  , mockLog
  ) where

import           HelVM.HelMA.Automaton.API.IOTypes
import qualified HelVM.HelMA.Automaton.API.LogLevel as LogLevel

import           Control.Monad.Logger
import           Control.Monad.Writer.Class         (MonadWriter, tell)

import           Data.MonoTraversable

import qualified Data.Sequence                      as Seq

filterLogsWithLevelInfo ∷ MockLoggerData → Output
filterLogsWithLevelInfo = filterLogsWithLevel LevelInfo

filterLogsWithLevelDebug ∷ MockLoggerData → Output
filterLogsWithLevelDebug = filterLogsWithLevel LevelDebug

filterLogsWithLevel ∷ LogLevel → MockLoggerData → Output
filterLogsWithLevel t logsSeq = oconcat (line <$> Seq.filter condition logsSeq) where
  condition l = t <= logLevel l
  line l = (LogLevel.showEitherTextLogLevel . LogLevel.fromLogger . logLevel) l <> " " <> (decodeUtf8 . fromLogStr . logStr) l <> "\n"

mockLog ∷ MonadMockLogger m ⇒ MockLog → m ()
mockLog = tell . one

----

type MonadMockLogger m = MonadWriter MockLoggerData m

type MockLoggerData = Seq MockLog

data MockLog = MockLog
  { logLoc    :: !Loc
  , logSource :: !LogSource
  , logLevel  :: !LogLevel
  , logStr    :: !LogStr
  }
  deriving stock (Eq, Show)
