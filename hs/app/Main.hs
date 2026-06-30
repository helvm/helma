module Main where

import           Env
import           Options

import           HelVM.HelMA

import qualified HelVM.HelMA.Automaton.API.AppOptions as App

import           HelVM.HelIO.Control.Control

import           Control.Monad.Writer.Lazy            (runWriterT)

import           HelVM.HelIO.Control.Message

import qualified Data.DList                           as D
import           Options.Applicative
import qualified RIO
import           RIO                                  (logOptionsHandle, runRIO, withLogFunc)

import           System.Environment                   (getProgName)
import qualified System.IO                            as IO

main :: IO ()
main = do
  progName <- getProgName
  opts     <- execParser (optsInfo progName)
  hSetBuffering stdout IO.NoBuffering
  logOptions <- logOptionsHandle stderr True
  withLogFunc logOptions (`runApp` opts)
  exitSuccess

optsInfo :: String -> ParserInfo App.AppOptions
optsInfo progName = info (optionsParser <**> helper <**> versionInfo progName)
  (  fullDesc
  <> progDesc "Runs esoteric programs - complete with pretty bad error messages"
  <> header (progName <> ": The Interpreter of BrainFuck , ETA , LazyK , Piet , SubLeq , WhiteSpace , Zot")
  )

versionInfo :: String -> Parser (a -> a)
versionInfo _ = infoOption "1.0.0"
  (  long "version"
  <> help "print version information and exit")

runApp :: MonadIO m => RIO.LogFunc -> App.AppOptions -> m ()
runApp logFunc = runRIO (Env logFunc) . runAsRIO . actualMain

runAsRIO :: (MonadIO m, MonadReader env m, RIO.HasLogFunc env) => ControlT m a -> m a
runAsRIO rio = do
  (result, logs) <- runWriterT $ runExceptT rio
  forM_ (D.toList logs) (RIO.logInfo . RIO.display)
  either (\err -> RIO.logError (RIO.display $ errorsToText err) >> RIO.exitFailure) pure result
