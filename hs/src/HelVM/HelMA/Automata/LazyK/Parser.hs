module HelVM.HelMA.Automata.LazyK.Parser where

import           HelVM.HelMA.Automata.LazyK.API.ParserType

import qualified HelVM.HelMA.Automata.LazyK.Parsers.CombinatorParser as CombinatorParser
import qualified HelVM.HelMA.Automata.LazyK.Parsers.MixedParser      as MixedParser
import qualified HelVM.HelMA.Automata.LazyK.Parsers.UnLambdaParser   as UnLambdaParser

import           HelVM.HelMA.Automata.LazyK.Lambda

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Safe

parse :: MonadSafe m => ParserType -> Source -> m Lambda
parse Mixed      = MixedParser.parse
parse UnLambda   = UnLambdaParser.parse
parse Combinator = CombinatorParser.parse
parse parserType = \ _ -> error $ show parserType
