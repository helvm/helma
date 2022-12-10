module HelVM.HelMA.Automata.ETA.Automaton (
  simpleRun,
  runWithParams,
  run,
) where

import           HelVM.HelMA.Automata.ETA.Evaluator
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Optimizer
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import qualified HelVM.HelMA.Automaton.Automaton          as Automaton

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.Collections.SList            as SList

import           Prelude                                  hiding (divMod)

import qualified Data.Sequence                            as Seq
import qualified Data.Vector                              as Vector

import           HelVM.HelMA.Automata.ETA.API.ETAImplType

simpleRun :: BIO m => (ETAImplType , Source , StackType) -> m ()
simpleRun (c , s , t) = run c s t (Just $ fromIntegral (maxBound :: Int)) Pretty

----

runWithParams :: BIO m => ETAImplType -> RunParams -> m ()
runWithParams e p = run e (source p) (stackTypeOptions p) Nothing (dumpTypeOptions p)

run :: (Evaluator Symbol m) => ETAImplType -> Source -> StackType -> Maybe Natural -> DumpType -> m ()
run etaImplType source = evalTL etaImplType (tokenize source)

evalTL :: (Evaluator Symbol m) => ETAImplType -> TokenList -> StackType -> Maybe Natural -> DumpType -> m ()
evalTL c tl ListStackType  = start c tl []
evalTL c tl SeqStackType   = start c tl Seq.empty
evalTL c tl SListStackType = start c tl SList.sListEmpty

start :: (SEvaluator Symbol s m) => ETAImplType -> TokenList -> s -> Maybe Natural -> DumpType -> m ()
start Original tl s _     dt = logDump dt =<< next (IU (Vector.fromList tl) 0) s
start Fast     tl s limit dt = Automaton.startWithIL s [] limit dt =<< optimize tl
