module HelVM.HelMA.Automata.False.ThunderSeethe.Evaluator.LLEvaluator where

import           HelVM.HelMA.Automata.False.ThunderSeethe.Evaluator
import           HelVM.HelMA.Automata.False.ThunderSeethe.Parser
import           HelVM.HelMA.Automata.False.Value

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

eval :: BIO m => Source -> m ()
eval source = do
  _ <- exec parsed [] emptyReg
  pass
   where parsed = (parse $ toString source) :: [Value Integer]
