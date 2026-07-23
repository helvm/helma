module HelVM.HelMA.Automaton.Extra where

import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Message

import           HelVM.HelIO.Control.Safe

import           Control.Monad.Logger

import qualified RIO

runAsRIO :: (MonadIO m, MonadReader env m, Has env) => LoggingT (SafeT m) a -> m a
runAsRIO action = do
  logFunc <- RIO.view RIO.logFuncL
  let logOutput _ source level msg =  RIO.runRIO logFunc $ RIO.logGeneric source (toRioLevel level) (RIO.displayBytesUtf8 $ fromLogStr msg)
  result <- runExceptT $ runLoggingT action logOutput
  either ((*> RIO.exitFailure) . RIO.logError . RIO.display . errorsToText) pure result

readSourceFile :: Has env => Exec -> String -> RIO.RIO env Source
readSourceFile True = pure . toText
readSourceFile _    = readTextFileRio

fallback :: Has env => EvalParams -> RIO.RIO env ()
fallback = putLTextLnRio . show . source

toRioLevel :: LogLevel -> RIO.LogLevel
toRioLevel  LevelError    = RIO.LevelError
toRioLevel  LevelWarn     = RIO.LevelWarn
toRioLevel  LevelInfo     = RIO.LevelInfo
toRioLevel  LevelDebug    = RIO.LevelDebug
toRioLevel (LevelOther l) = RIO.LevelOther l
