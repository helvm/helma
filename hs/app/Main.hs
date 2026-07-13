module Main where

import           Options

import           HelVM.HelMA.Evaluator

import           HelVM.HelIO.Extra                    (readFileTextUtf8)
import qualified HelVM.HelMA.Automaton.API.AppOptions as App
import           HelVM.HelMA.Automaton.API.Env

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
runApp logFunc = runRIO (productionEnv logFunc) . runWithOptions

productionEnv :: RIO.LogFunc -> Env
productionEnv logFunc = Env logFunc productionFileIO

productionFileIO :: FileIO
productionFileIO = FileIO
  { readTextFile = readFileTextUtf8
  }
