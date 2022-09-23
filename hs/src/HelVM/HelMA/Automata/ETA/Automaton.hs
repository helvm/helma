module HelVM.HelMA.Automata.ETA.Automaton (
  simpleRun,
  runWithParams,
  run,
) where

import           HelVM.HelMA.Automata.ETA.Evaluator
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.Collections.SList           as SList

import           Prelude                                 hiding (divMod)

import qualified Data.Sequence                           as Seq
import qualified Data.Vector                             as Vector

simpleRun :: BIO m => (Bool , Source , StackType) -> m ()
simpleRun (c , s , t) = run c s t Pretty

----

runWithParams :: BIO m => RunParams -> m ()
runWithParams p = run (compile p) (source p) (stackTypeOptions p) (dumpTypeOptions p)

run :: (Evaluator Symbol m) => Bool -> Source -> StackType -> DumpType -> m ()
run compile source = evalTL compile (tokenize source)

evalTL ::  (Evaluator Symbol m) => Bool -> TokenList -> StackType -> DumpType -> m ()
evalTL c tl ListStackType  = start c tl []
evalTL c tl SeqStackType   = start c tl Seq.empty
evalTL c tl SListStackType = start c tl SList.sListEmpty

start :: (SEvaluator Symbol s m) => Bool -> TokenList -> s -> DumpType -> m ()
start _ tl s dt = logDump dt =<< next (IU (Vector.fromList tl) 0) s --FIXME https://github.com/helvm/helma/issues/83
