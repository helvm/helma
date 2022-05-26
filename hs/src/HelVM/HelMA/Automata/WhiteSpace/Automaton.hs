module HelVM.HelMA.Automata.WhiteSpace.Automaton (
  simpleEval,
  evalParams,
  eval,
  evalIL,
  evalTL,
  start,
) where

import           HelVM.HelMA.Automata.WhiteSpace.Evaluator
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import           HelVM.HelMA.Automata.WhiteSpace.Symbol
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Units.CPU              as CPU

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Safe

import           Prelude                                      hiding (swap)

import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams as S

import qualified HelVM.HelIO.Collections.MapList              as MapList
import qualified HelVM.HelIO.Collections.SList                as SList

import qualified Data.Sequence                                as Seq
import qualified Data.Vector                                  as Vector

simpleEval :: BIO m => S.SimpleParams -> m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.asciiLabel p) (S.stackType p) (S.ramType p) (S.dumpType p)

----

evalParams :: BIO m => TokenType -> EvalParams -> m ()
evalParams tokenType p = eval tokenType (source p) (asciiLabel p) (stackTypeOptions p) (ramTypeOptions p) (dumpTypeOptions p)

eval :: BIO m => TokenType -> Source -> Bool -> StackType -> RAMType -> DumpType -> m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: BIO m => TokenList -> Bool -> StackType -> RAMType -> DumpType -> m ()
evalTL tl ascii st rt dt = evalTL' =<< liftSafe (parseTL ascii tl) where evalTL' il = evalIL il st rt dt

evalIL :: BIO m => InstructionList -> StackType -> RAMType -> DumpType -> m ()
evalIL il s ListRAMType    = evalIL' il s []
evalIL il s SeqRAMType     = evalIL' il s Seq.empty
evalIL il s SListRAMType   = evalIL' il s SList.sListEmpty
evalIL il s MapListRAMType = evalIL' il s MapList.mapListEmpty

evalIL' :: (REvaluator Symbol r m) => InstructionList -> StackType -> r -> DumpType -> m ()
evalIL' il ListStackType  = start il []
evalIL' il SeqStackType   = start il Seq.empty
evalIL' il SListStackType = start il SList.sListEmpty

start :: (SREvaluator Symbol s r m) => InstructionList -> s -> r -> DumpType -> m ()
start il s r dt = logDump dt =<< next (CU (Vector.fromList il) 0 (IS [])) s r
