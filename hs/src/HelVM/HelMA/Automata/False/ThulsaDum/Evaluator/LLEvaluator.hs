module HelVM.HelMA.Automata.False.ThulsaDum.Evaluator.LLEvaluator where

import           HelVM.HelMA.Automata.False.ThulsaDum.Evaluator
import           HelVM.HelMA.Automata.False.ThulsaDum.Parser

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           Text.Parsec

eval :: BIO m => Source -> m ()
eval = exec . toString

exec :: BIO m => String -> m ()
exec conts = case parse parser "<console>" conts of
           Left msg ->
             wLogStrLn $ "parser error: " <> show msg
           Right ts -> do
             ret <- runEval ts
             case ret of
               Left msg    -> wLogStrLn $ "runtime error: " <> toText msg
               Right stack -> wLogStrLn $ show stack
         where parser :: FalseParser Int32 = parseFalse
