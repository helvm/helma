module Main where

import           Options

import           HelVM.HelMA

import qualified HelVM.HelMA.Automaton.API.AppOptions as App

import           HelVM.HelMA.Automaton.Types.FileType

import           HelVM.HelIO.Control.Control

import           Options.Applicative

import           System.Environment                   (getProgName)
import qualified System.IO                            as IO

main :: IO ()
main = do
  progName <- getProgName
  opts     <- execParser (optsInfo progName)
  setNoBuffering
  actualMain opts
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

setNoBuffering :: IO ()
setNoBuffering = hSetBuffering stdout IO.NoBuffering

actualMain :: App.AppOptions -> IO ()
actualMain = runNoBuffering =<< App.fileType

runNoBuffering :: FileType -> App.AppOptions -> IO ()
runNoBuffering BinaryFile o = runBinary o
runNoBuffering TextFile   o = controlTToIO (App.printLogs o) $ runText o

runBinary :: App.AppOptions -> IO ()
-- runBinary o = Piet.actualMain $ Piet.PietOptions { program = Just $ App.file o, codelSize = App.codelSize o, verbosity = App.verbosity o }
runBinary o = controlTToIO (App.printLogs o) $ runText o
