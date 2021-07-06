module HelVM.HelMA.Automata.ETA.Evaluator (
  uncurryEval,
  evalParams,
  eval
) where

import HelVM.HelMA.Automata.ETA.Addressing
import HelVM.HelMA.Automata.ETA.Lexer
import HelVM.HelMA.Automata.ETA.OperandParsers
import HelVM.HelMA.Automata.ETA.Symbol
import HelVM.HelMA.Automata.ETA.Token

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.TypeOptions
import HelVM.HelMA.Automaton.IO.BusinessIO
import HelVM.HelMA.Automaton.Memories.StackConst as Stack
import HelVM.HelMA.Automaton.Types.StackType

import HelVM.Common.Safe
import HelVM.Common.Util

import Prelude hiding (divMod)

import qualified Data.Sequence as Seq

uncurryEval :: (Evaluator Symbol m) => (Source , StackType) -> m ()
uncurryEval = uncurry eval

----

evalParams :: (Evaluator Symbol m) => EvalParams -> m ()
evalParams p = eval (source p) (stack $ typeOptions p)

eval :: (Evaluator Symbol m) => Source -> StackType -> m ()
eval source = evalTL (tokenize source)

evalTL ::  (Evaluator Symbol m) => TokenList -> StackType -> m ()
evalTL tl ListStackType = start tl []
evalTL tl SeqStackType  = start tl Seq.empty

start :: (SEvaluator Symbol s m) => TokenList -> s -> m ()
start il = next (IU il 0)

next :: (SEvaluator e s m) => InstructionUnit -> s -> m ()
next iu s = doInstruction' =<< liftSafe (nextIU iu)  where doInstruction' (t , iu') = doInstruction t iu' s

doInstruction :: (SEvaluator e s m) => Maybe Token -> InstructionUnit -> s -> m ()
-- IO instructions
doInstruction (Just O) iu s = doOutputChar iu s
doInstruction (Just I) iu s = doInputChar  iu s

-- Stack instructions
doInstruction (Just N) iu s = next' =<< liftSafe (parseNumber iu) where next' (symbol , iu') = next iu' (push1 symbol s)
doInstruction (Just H) iu s = next iu =<< liftSafe (halibut s)

-- Arithmetic
doInstruction (Just S) iu s = next iu =<< liftSafe (sub s)
doInstruction (Just E) iu s = next iu =<< liftSafe (divMod s)

-- Control
doInstruction (Just R) iu s = next iu s
doInstruction (Just A) iu@(IU il ic) s = next iu $ flipPush1 s $ genericNextLabel il ic
doInstruction (Just T) iu@(IU il _ ) s = transfer =<< liftSafe (pop2 s) where
  transfer (_ , 0 , s') = next iu s'
  transfer (0 , _ , _ ) = doEnd iu s
  transfer (l , _ , s') = next' =<< liftSafe (genericFindAddress il l) where next' address = next (IU il address) s'
doInstruction Nothing iu s = doEnd iu s

-- IO instructions
doOutputChar :: (SEvaluator e s m) => InstructionUnit -> s -> m ()
doOutputChar iu s = doOutputChar' =<< liftSafe (pop1 s) where
  doOutputChar' (e , s') = wPutChar (genericChr e) *> next iu s'

doInputChar :: (SEvaluator e s m) => InstructionUnit -> s -> m ()
doInputChar iu s = doInputChar' =<< wGetChar where
  doInputChar' char = next iu $ charPush1 char s

-- Terminate instruction
doEnd :: (SEvaluator e s m) => InstructionUnit -> s -> m ()
doEnd iu s = wLogShow iu *> wLogShow s
