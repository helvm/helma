module Main where

import           Options

import           HelVM.HelMA

import qualified HelVM.HelMA.Automaton.API.AppOptions as App

import           HelVM.HelIO.Control.Control

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
runNoBuffering False o = controlTToIO (App.printLogs o) $ runText o
runNoBuffering True  o = controlTToIO (App.printLogs o) $ runText o
