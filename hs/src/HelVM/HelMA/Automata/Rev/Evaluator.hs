module HelVM.HelMA.Automata.Rev.Evaluator (
  evalParams,
  eval
) where

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

import qualified Data.Text                            as Text

evalParams :: BIO m => EvalParams -> m ()
evalParams = eval . source

eval :: BusinessIO m => Source -> m ()
eval = evalLines . lines

evalLines :: BusinessIO m => [Source] -> m ()
evalLines ll = doOutput $ unlines $ Text.reverse <$> ll

doOutput :: BusinessIO m => Source -> m ()
doOutput = wPutStr
