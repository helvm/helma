module HelVM.HelMA.Automata.ETA.Automaton (
  simpleEval,
  evalParams,
  eval,
) where

import           HelVM.HelMA.Automata.ETA.Evaluator
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.Common.Collections.SList          as SList

import           Prelude                                 hiding (divMod)

import qualified Data.Sequence                           as Seq
import qualified Data.Vector                             as Vector

simpleEval :: BIO m => (Bool , Source , StackType) -> m ()
simpleEval (c , s , t) = eval c s t Pretty

----

evalParams :: BIO m => EvalParams -> m ()
evalParams p = eval (compile p) (source p) (stackTypeOptions p) (dumpTypeOptions p)

eval :: (Evaluator Symbol m) => Bool -> Source -> StackType -> DumpType -> m ()
eval compile source = evalTL compile (tokenize source)

evalTL ::  (Evaluator Symbol m) => Bool -> TokenList -> StackType -> DumpType -> m ()
evalTL c tl ListStackType  = start c tl []
evalTL c tl SeqStackType   = start c tl Seq.empty
evalTL c tl SListStackType = start c tl SList.sListEmpty

start :: (SEvaluator Symbol s m) => Bool -> TokenList -> s -> DumpType -> m ()
start _ tl s dt = logDump dt =<< next (IU (Vector.fromList tl) 0) s
