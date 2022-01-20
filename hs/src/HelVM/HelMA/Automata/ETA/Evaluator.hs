module HelVM.HelMA.Automata.ETA.Evaluator (
  simpleEval,
  evalParams,
  eval
) where

import           HelVM.HelMA.Automata.ETA.Addressing
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Units.ALU         as Stack

import           HelVM.Common.Collections.SList          as SList

import           HelVM.Common.Control.Logger

import           Prelude                                 hiding (divMod)

import qualified Data.Sequence                           as Seq
import qualified Data.Vector                             as Vector

simpleEval :: (Evaluator Symbol m) => (Bool , Source , StackType) -> m ()
simpleEval (c , s , t) = eval c s t

----

evalParams :: (Evaluator Symbol m) => EvalParams -> m ()
evalParams p = eval (compile p) (source p) (stackTypeOptions p)

eval :: (Evaluator Symbol m) => Bool -> Source -> StackType -> m ()
eval compile source = evalTL compile (tokenize source)

evalTL ::  (Evaluator Symbol m) => Bool -> TokenList -> StackType -> m ()
evalTL c tl ListStackType  = start c tl []
evalTL c tl SeqStackType   = start c tl Seq.empty
evalTL c tl SListStackType = start c tl SList.sListEmpty

start :: (SEvaluator Symbol s m) => Bool -> TokenList -> s -> m ()
start _ tl = next (IU (Vector.fromList tl) 0)

next :: (SEvaluator e s m) => InstructionUnit -> s -> m ()
next iu s = doInstruction' =<< nextIU iu  where doInstruction' (t , iu') = doInstruction t iu' s

doInstruction :: (SEvaluator e s m) => Maybe Token -> InstructionUnit -> s -> m ()
-- | IO instructions
doInstruction (Just O) iu s = next iu =<< doOutputChar2 s
doInstruction (Just I) iu s = next iu =<< doInputChar2 s

-- | Stack instructions
doInstruction (Just N) iu s = next' =<< parseNumber iu where next' (symbol , iu') = next iu' (push1 symbol s)
doInstruction (Just H) iu s = next iu =<< halibut s

-- | Arithmetic
doInstruction (Just S) iu s = next iu =<< sub s
doInstruction (Just E) iu s = next iu =<< divMod s

-- | Control
doInstruction (Just R) iu s = next iu s
doInstruction (Just A) iu@(IU il ic) s = (next iu . flipPush1 s . genericNextLabel il) ic
doInstruction (Just T) iu@(IU il _ ) s = transfer =<< pop2 s where
  transfer (_ , 0 , s') = next iu s'
  transfer (0 , _ , _ ) = doEnd iu s
  transfer (l , _ , s') = next' =<< genericFindAddress il l where next' address = next (IU il address) s'
doInstruction Nothing iu s = doEnd iu s

-- | Terminate instruction
doEnd :: (SEvaluator e s m) => InstructionUnit -> s -> m ()
doEnd iu s = logMessageTuple ("iu" , show iu) *> logMessageTuple ("stack" , show s)
