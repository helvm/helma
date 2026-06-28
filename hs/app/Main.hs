module Main where

import           Options

import           HelVM.HelMA

import qualified HelVM.HelMA.Automaton.API.AppOptions as App
import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Control

import           HelVM.HelIO.Extra

import           Options.Applicative

import qualified System.IO                            as IO

main :: IO ()
main = runApp =<< execParser opts where
  opts = info (optionsParser <**> helper)
      ( fullDesc
     <> header "HelMA: The Interpreter of BrainFuck , ETA , LazyK , SubLeq , WhiteSpace, Zot"
     <> progDesc "Runs esoteric programs - complete with pretty bad error messages" )

runApp :: App.AppOptions -> IO ()
runApp o = do
  setNoBuffering
  (runNoBuffering =<< App.isImage) o

setNoBuffering :: IO ()
setNoBuffering = hSetBuffering stdout IO.NoBuffering

runNoBuffering :: Bool -> App.AppOptions -> IO ()
runNoBuffering False = runText
runNoBuffering True  = runText

runText :: App.AppOptions -> IO ()
runText o = do
  source <- readSourceFile (App.exec o) (App.file o)
  controlTToIO (App.printLogs o) $ run (App.emit o) (App.langWithOptions o) (App.evalParams o source)

readSourceFile :: Exec -> String -> IO Source
readSourceFile True = pure . toText
readSourceFile _    = readFileTextUtf8
