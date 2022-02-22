module HelVM.HelMA.Automaton.IO.EvaluatorIO (
  SREvaluator,
  REvaluator,
  SEvaluator,
  Evaluator,
) where

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Units.ALU
import           HelVM.HelMA.Automaton.Units.RAM

type SREvaluator e s r m = (Stack s e, RAM r e, Evaluator e m)
type REvaluator  e r m   = (RAM r e, Evaluator e m)
type SEvaluator  e s m   = (Stack s e, Evaluator e m)
type Evaluator   e m     = (Element e , BIO m)
