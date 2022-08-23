module HelVM.Common.Control.Logger (
  loggerIOToPTextIO,
  loggerIOToIO,
  loggerToIO,
  removeLoggerT,
  removeLogger,
  runLoggerT,
  runLogger,

  logsFromLoggerT,
  logsFromLogger,

  loggerT,
  logger,
  withMessages,

  liftLogger,

  logMessageTupleList,
  logMessageTuple,

  logData,
  logMessage,
  logMessages,

  MonadLogger,
  LoggerT,
  Logger,
  WithMessages,
) where

import           HelVM.Common.Control.Message

import           Control.Monad.Writer.Lazy

import           HelVM.Common.Util

import qualified Data.DList                   as D

-- | DeConstructors
loggerIOToPTextIO :: Show a => IO (Logger a) -> IO Text
loggerIOToPTextIO a = showP <$> loggerIOToIO a

loggerIOToIO :: IO (Logger a) -> IO a
loggerIOToIO a = loggerToIO =<< a

loggerToIO :: Logger a -> IO a
loggerToIO = pure . removeLogger

removeLoggerT :: Monad m => LoggerT m a -> m a
removeLoggerT a = fst <$> runWriterT a

removeLogger :: Logger a -> a
removeLogger = fst . runWriter

runLoggerT :: LoggerT m a -> m (a , Messages)
runLoggerT = runWriterT

runLogger :: Logger a -> (a , Messages)
runLogger = runWriter

-- | Logs
logsFromLoggerT :: Monad m => LoggerT m a -> m Messages
logsFromLoggerT a = snd <$> runWriterT a

logsFromLogger :: Logger a -> Messages
logsFromLogger = snd . runWriter

-- | Constructors
loggerT :: Monad m => m a -> LoggerT m a
loggerT a = WriterT $ withMessages <$> a

logger :: a -> Logger a
logger a = WriterT  $ Identity $ withMessages a

withMessages :: a -> WithMessages a
withMessages a = (a , D.empty)

-- | Lift
liftLogger :: MonadLogger m => Logger a -> m a
liftLogger = writer . runWriter

-- | Append Messages
logMessageTupleList :: MonadLogger m => [MessageTuple] -> m ()
logMessageTupleList = logMessage . tupleListToMessage

logMessageTuple :: MonadLogger m => MessageTuple -> m ()
logMessageTuple = logMessage . logTupleToMessage

logTupleToMessage :: MessageTuple -> Message
logTupleToMessage (k , v) = k <> ": " <> v

logData :: (MonadLogger m , Show a) => a -> m ()
logData = logMessage . show

logMessage :: MonadLogger m => Message -> m ()
logMessage = logMessages . D.singleton

logMessages :: MonadLogger m => Messages -> m ()
logMessages = tell

-- | Types
type MonadLogger m = MonadWriter Messages m

type LoggerT m = WriterT Messages m

type Logger = Writer Messages

type WithMessages a = (a , Messages)
