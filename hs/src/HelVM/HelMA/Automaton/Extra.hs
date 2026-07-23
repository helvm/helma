module HelVM.HelMA.Automaton.Extra where

import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe

import qualified RIO

runAsRIO :: (MonadIO m, MonadReader env m, Has env) => SafeT m a -> m a
runAsRIO action = do
  result <- runExceptT action
  either ((*> RIO.exitFailure) . RIO.logError . RIO.display . errorsToText) pure result

readSourceFile :: Has env => Exec -> String -> RIO.RIO env Source
readSourceFile True = pure . toText
readSourceFile _    = readTextFileRio

fallback :: Has env => EvalParams -> RIO.RIO env ()
fallback = putLTextLnRio . show . source
