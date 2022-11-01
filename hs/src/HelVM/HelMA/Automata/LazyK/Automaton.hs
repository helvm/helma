module HelVM.HelMA.Automata.LazyK.Automaton (
  runWithParams,
  run,
  runReduce,
) where

import           HelVM.HelMA.Automata.LazyK.Evaluator
import           HelVM.HelMA.Automata.LazyK.InputEncoder
import           HelVM.HelMA.Automata.LazyK.Lambda
import           HelVM.HelMA.Automata.LazyK.Parser

import           HelVM.HelMA.Automata.LazyK.Reducer

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import           HelVM.HelMA.Automaton.IO.BusinessIO

runWithParams :: BIO m => RunParams -> m ()
runWithParams = run . source

run :: BIO m => Source -> m ()
run = runLambda <=< parse

runReduce :: BIO m => Source -> m Source
runReduce s = show . reduce <$> parse s

runLambda :: BIO m => Lambda -> m ()
runLambda lambda = (eval . reduce . App lambda . readInput) =<< wGetContentsBS
