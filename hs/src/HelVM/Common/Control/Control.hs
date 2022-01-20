module HelVM.Common.Control.Control (
  controlTToIO,
  controlTToIOWithoutLogs,
  controlTToIOWithLogs,
  controlToIO,

  runControlT,
  runControl,

  safeWithMessagesToText,

  controlT,
  control,

  safeWithMessages,

  MonadControl,
  ControlT,
  Control,

  UnitSafeWithMessages,
  SafeWithMessages
) where

import           HelVM.Common.Control.Logger
import           HelVM.Common.Control.Message
import           HelVM.Common.Control.Safe

import           Control.Type.Operator

import qualified System.IO                    as IO

controlTToIO :: Bool -> ControlT IO a -> IO a
controlTToIO False = controlTToIOWithoutLogs
controlTToIO True  = controlTToIOWithLogs

controlTToIOWithoutLogs :: ControlT IO a -> IO a
controlTToIOWithoutLogs a = safeWithMessagesToIOWithoutLogs =<< runControlT a

controlTToIOWithLogs :: ControlT IO a -> IO a
controlTToIOWithLogs a = safeWithMessagesToIOWithLogs =<< runControlT a

controlToIO :: Control a -> IO a
controlToIO = safeToIO . removeLogger

runControlT :: ControlT m a -> m $ SafeWithMessages a
runControlT = runLoggerT . runSafeT

runControl :: Control a -> SafeWithMessages a
runControl a = runLogger $ runSafe <$> a

safeWithMessagesToIOWithoutLogs :: SafeWithMessages a -> IO a
safeWithMessagesToIOWithoutLogs (safe , _) = safeToIO safe

safeWithMessagesToIOWithLogs :: SafeWithMessages a -> IO a
safeWithMessagesToIOWithLogs (safe , logs) = safeToIO safe <* IO.hPutStr stderr (errorsToString logs)

safeWithMessagesToText :: SafeWithMessages a -> Text
safeWithMessagesToText (safe , messages) = errorsToText messages <> safeToText safe

-- | Constructors
controlT :: Monad m => m a -> ControlT m a
controlT = safeT . loggerT

control :: a -> Control a
control = logger . pure

safeWithMessages :: a -> SafeWithMessages a
safeWithMessages = withMessages . pure

-- | Types
type MonadControl m = (MonadLogger m, MonadSafe m)

type ControlT m = SafeT (LoggerT m)

type Control a = Logger $ Safe a

type UnitSafeWithMessages = SafeWithMessages ()

type SafeWithMessages a = WithMessages (Safe a)
