module HelVM.HelMA.Automata.LazyK.Evaluator (
  run,
  evalParams,
  evalSource,
  reduceSource,
) where

import           HelVM.HelMA.Automata.LazyK.Automaton
import           HelVM.HelMA.Automata.LazyK.InputEncoder
import           HelVM.HelMA.Automata.LazyK.Lambda
import           HelVM.HelMA.Automata.LazyK.Parser

import           HelVM.HelMA.Automata.LazyK.Reducer

import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

run :: AppEff m => Emit -> EvalParams -> m ()
run No = evalParams
run _  = fallback

evalParams :: AppEff m => EvalParams -> m ()
evalParams = evalSource . source

evalSource :: AppEff m => Source -> m ()
evalSource = evalLambda <=< parse

evalLambda :: AppEff m => Lambda -> m ()
evalLambda lambda = (runAutomat . reduce . App lambda . readInput) =<< eGetContentsBS

reduceSource :: AppEff m => Source -> m Source
reduceSource s = show . reduce <$> parse s
