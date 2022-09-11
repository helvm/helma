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

import           HelVM.HelMA.Automata.LazyK.API.ParserType

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import           HelVM.HelMA.Automaton.IO.BusinessIO

runWithParams :: BIO m => ParserType -> RunParams -> m ()
runWithParams parserType = run parserType . source

run :: BIO m => ParserType -> Source -> m ()
run parserType s = runLambda =<< parse parserType s

runReduce :: BIO m => ParserType -> Source -> m Source
runReduce parserType s = show . reduce <$> parse parserType s

runLambda :: BIO m => Lambda -> m ()
runLambda lambda = (eval . reduce . App lambda . readInput) =<< wGetContentsBS
