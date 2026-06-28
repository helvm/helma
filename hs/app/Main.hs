module Main where

import           Options

import           HelVM.HelMA

import qualified HelVM.HelMA.Automaton.API.AppOptions as App
import           HelVM.HelMA.Automaton.API.BoolTypes
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.Lang

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
  run (App.emit o) (App.printLogs o) (App.langWithOptions o) (App.evalParams o source)

readSourceFile :: Exec -> String -> IO Source
readSourceFile True = pure . toText
readSourceFile _    = readFileTextUtf8

run :: Emit -> PrintLogs -> LangWithOptions -> EvalParams -> IO ()
run No   p l r = (controlTToIO p . evalParams l) r
run IL   _ l r = putLTextLn $ parse          l (formatType r) (source r)
run TL   _ l r = putTextLn  $ tokenize       l (source r)
run Code _ l r = putTextLn  $ minification   l (source r)
