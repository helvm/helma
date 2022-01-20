module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  simpleEval,
  simpleEvalTL,
  evalParams,
  eval,
  evalIL,
  evalTL,
  start,
) where

import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import           HelVM.HelMA.Automata.WhiteSpace.Symbol
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Units.ALU              as Stack
import           HelVM.HelMA.Automaton.Units.CPU              as CPU
import           HelVM.HelMA.Automaton.Units.LSU              as LSU

import           HelVM.Common.Containers.LLIndexSafe
import           HelVM.Common.Control.Logger
import           HelVM.Common.Control.Safe

import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType


import           Prelude                                      hiding (swap)

import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams as S

import qualified HelVM.Common.Collections.MapList             as MapList
import qualified HelVM.Common.Collections.SList               as SList

import qualified Data.Sequence                                as Seq
import qualified Data.Vector                                  as Vector


simpleEval :: (Evaluator Symbol m) => S.SimpleParams -> m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.asciiLabel p) (S.stackType p) (S.ramType p)

simpleEvalTL :: (Evaluator Symbol m) => TokenList -> m ()
simpleEvalTL tl = evalTL tl False defaultStackType defaultRAMType

----

evalParams :: (Evaluator Symbol m) => TokenType -> EvalParams -> m ()
evalParams tokenType p = eval tokenType (source p) (asciiLabel p) (stackTypeOptions p) (ramTypeOptions p)

eval :: (Evaluator Symbol m) => TokenType -> Source -> Bool -> StackType -> RAMType -> m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: (Evaluator Symbol m) => TokenList -> Bool -> StackType -> RAMType -> m ()
evalTL tl ascii st rt = evalTL' =<< liftSafe (parseTL ascii tl) where evalTL' il = evalIL il st rt

evalIL :: (Evaluator Symbol m) => InstructionList -> StackType -> RAMType -> m ()
evalIL il s ListRAMType    = evalIL' il s []
evalIL il s SeqRAMType     = evalIL' il s Seq.empty
evalIL il s SListRAMType   = evalIL' il s SList.sListEmpty
evalIL il s MapListRAMType = evalIL' il s MapList.mapListEmpty

evalIL' :: (REvaluator Symbol r m) => InstructionList -> StackType -> r -> m ()
evalIL' il ListStackType  = start il []
evalIL' il SeqStackType   = start il Seq.empty
evalIL' il SListStackType = start il SList.sListEmpty

start :: (SREvaluator Symbol s r m) => InstructionList -> s -> r -> m ()
start il = next (CU (Vector.fromList il) 0 (IS []))

next :: (SREvaluator Symbol s r m) => ControlUnit -> s -> r -> m ()
next (CU il ic is) s r = doInstruction' =<< indexSafe il ic where doInstruction' i = doInstruction i (CU il (ic+1) is) s r

stackNext :: (SREvaluator Symbol s r m) => ControlUnit -> r -> s -> m ()
stackNext cu r s = next cu s r

cuNext :: (SREvaluator Symbol s r m) => r -> ControlUnit -> s -> m ()
cuNext r cu s = next cu s r

----

doInstruction :: (SREvaluator Symbol s r m) => Instruction -> ControlUnit -> s -> r -> m ()
doInstruction (IAL      i) cu s r = stackNext cu r =<< alInstruction i s
doInstruction (ILS      i) cu s r = uncurry (next cu) . sluToTuple =<< slInstruction i (LSU s r)
doInstruction (IControl i) cu s r = uncurry (cuNext r). cpuToTuple =<< controlInstruction i (CPU cu s)
doInstruction  End         cu s r = logMessageTuple ("cu" , show cu) *> logMessageTuple ("stack" , show s) *> logMessageTuple ("ram" , show r)
