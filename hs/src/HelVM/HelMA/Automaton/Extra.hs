module HelVM.HelMA.Automaton.Extra where

import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Message

import           Control.Monad.Writer.Lazy            (runWriterT)

import qualified Data.DList                           as D

import qualified RIO

runAsRIO :: (MonadIO m, MonadReader env m, Has env) => ControlT m a -> m a
runAsRIO rio = do
  (result, logs) <- runWriterT $ runExceptT rio
  forM_ (D.toList logs) (RIO.logInfo . RIO.display)
  either ((*> RIO.exitFailure) . RIO.logError . RIO.display . errorsToText) pure result

readSourceFile :: Has env => Exec -> String -> RIO.RIO env Source
readSourceFile True = pure . toText
readSourceFile _    = readTextFileRio

fallback :: Has env => EvalParams -> RIO.RIO env ()
fallback = putLTextLnRio . show . source
