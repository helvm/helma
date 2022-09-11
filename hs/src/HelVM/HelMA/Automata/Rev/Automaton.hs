module HelVM.HelMA.Automata.Rev.Automaton (
  runWithParams,
  run
) where

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams
import           HelVM.HelMA.Automaton.IO.BusinessIO

import qualified Data.Text                           as Text

runWithParams :: BIO m => RunParams -> m ()
runWithParams = run . source

run :: BusinessIO m => Source -> m ()
run = evalLines . lines

evalLines :: BusinessIO m => [Source] -> m ()
evalLines ll = doOutput $ unlines $ Text.reverse <$> ll

doOutput :: BusinessIO m => Source -> m ()
doOutput = wPutStr
