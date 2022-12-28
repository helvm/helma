module HelVM.HelMA.Automata.LazyK.Evaluator (
  evalParams,
  evalSource,
  reduceSource,
) where

import           HelVM.HelMA.Automata.LazyK.Automaton
import           HelVM.HelMA.Automata.LazyK.InputEncoder
import           HelVM.HelMA.Automata.LazyK.Lambda
import           HelVM.HelMA.Automata.LazyK.Parser

import           HelVM.HelMA.Automata.LazyK.Reducer

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.BusinessIO

evalParams :: BIO m => EvalParams -> m ()
evalParams = evalSource . source

evalSource :: BIO m => Source -> m ()
evalSource = evalLambda <=< parse

evalLambda :: BIO m => Lambda -> m ()
evalLambda lambda = (run . reduce . App lambda . readInput) =<< wGetContentsBS

reduceSource :: BIO m => Source -> m Source
reduceSource s = show . reduce <$> parse s
