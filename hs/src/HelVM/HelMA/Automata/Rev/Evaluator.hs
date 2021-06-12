module HelVM.HelMA.Automata.Rev.Evaluator (
  evalParams,
  eval
) where

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.IO.WrapperIO

import HelVM.Common.SafeMonadT

import qualified Data.Text as Text

evalParams :: (Monad m , Evaluator (m ())) => EvalParams -> SafeMonadT_ m
evalParams = hoistMonad . eval . source

eval :: Evaluator r => Source ->  r
eval = evalLines . lines

evalLines :: Evaluator r => [Source] -> r
evalLines ll = doOutput $ unlines $ Text.reverse <$> ll

----

class Evaluator r where
  doOutput :: Source ->  r

----

instance WrapperIO m => Evaluator (m ()) where
  doOutput = wPutStr
