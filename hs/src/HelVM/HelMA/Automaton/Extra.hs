module HelVM.HelMA.Automaton.Extra where

import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Message

import           HelVM.HelIO.Control.Safe

import           Control.Monad.Logger

import qualified RIO

runAsRIO ∷ (MonadIO m, MonadReader env m, Has env) ⇒ LoggingT (SafeT m) a → m a
runAsRIO action = either (const RIO.exitFailure) pure =<< runAsRIOResult action

runAsRIOResult ∷ (MonadIO m, MonadReader env m, Has env) ⇒ LoggingT (SafeT m) a → m (Either Messages a)
runAsRIOResult action = go =<< runExceptT . runLoggingT action . logOutput =<< RIO.view RIO.logFuncL where
  go res = res <$ whenLeft_ res logSafeError

logOutput ∷ (MonadIO m, RIO.HasLogFunc env) ⇒ env → p → RIO.LogSource → LogLevel → LogStr → m ()
logOutput logFunc _ source level msg = RIO.runRIO logFunc $ RIO.logGeneric source (toRioLevel level) (RIO.displayBytesUtf8 $ fromLogStr msg)

logSafeError ∷ (MonadIO m, MonadReader env m, Has env) ⇒ Messages → m ()
logSafeError = RIO.logError . RIO.display . errorsToText

readSourceFile ∷ Exec → String → RIO.RIO Env Source
readSourceFile True = pure . toText
readSourceFile _    = readTextFileRio

fallback ∷ EvalParams → RIO.RIO Env ()
fallback = putLTextLnRio . show . source

toRioLevel ∷ LogLevel → RIO.LogLevel
toRioLevel  LevelError    = RIO.LevelError
toRioLevel  LevelWarn     = RIO.LevelWarn
toRioLevel  LevelInfo     = RIO.LevelInfo
toRioLevel  LevelDebug    = RIO.LevelDebug
toRioLevel (LevelOther l) = RIO.LevelOther l
