module HelVM.Common.Control.Control (
  controlTToIO,
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

controlTToIO :: ControlT IO a -> IO a
controlTToIO a = safeWithMessagesToIO =<< runControlT a

controlToIO :: Control a -> IO a
controlToIO = safeToIO . removeLogger

runControlT :: ControlT m a -> m $ SafeWithMessages a
runControlT = runLoggerT . runSafeT

runControl :: Control a -> SafeWithMessages a
runControl a = runLogger $ runSafe <$> a

safeWithMessagesToIO :: SafeWithMessages a -> IO a
safeWithMessagesToIO (safe , _) = safeToIO safe

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
