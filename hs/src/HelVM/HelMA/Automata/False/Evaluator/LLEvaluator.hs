module HelVM.HelMA.Automata.False.Evaluator.LLEvaluator where

import           HelVM.HelMA.Automata.False.Exec
import           HelVM.HelMA.Automata.False.Parser
import           HelVM.HelMA.Automata.False.Util

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

eval :: BIO m => Source -> m ()
--eval :: Source -> IO ()
eval source = do
  _ <- exec (parse $ toString source) [] emptyReg
  pure ()
