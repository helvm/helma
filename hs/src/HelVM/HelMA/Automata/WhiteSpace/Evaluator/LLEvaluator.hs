module HelVM.HelMA.Automata.WhiteSpace.Evaluator.LLEvaluator (
  simpleEval,
  simpleEvalTL,
  evalParams,
  eval,
  evalIL,
  evalTL
) where

import           HelVM.HelMA.Automata.WhiteSpace.Addressing
import           HelVM.HelMA.Automata.WhiteSpace.Instruction
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import           HelVM.HelMA.Automata.WhiteSpace.Symbol
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Memories.RAM.LLRAM          as RAM
import           HelVM.HelMA.Automaton.Memories.Stack.LLStack      as Stack


import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.LowControlOperator
import           HelVM.HelMA.Automaton.Operator.MemoryOperator

import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Logger
import           HelVM.HelIO.Control.Safe

import           Prelude                                           hiding (swap)

import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams      as S

import qualified HelVM.HelIO.Collections.MapList                   as MapList
import qualified HelVM.HelIO.Collections.SList                     as SList

import qualified Data.Sequence                                     as Seq
import qualified Data.Vector                                       as Vector

simpleEval :: (LLEvaluator Symbol m) => S.SimpleParams -> m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.asciiLabel p) (S.stackType p) (S.ramType p)

simpleEvalTL :: (LLEvaluator Symbol m) => TokenList -> m ()
simpleEvalTL tl = evalTL tl False defaultStackType defaultRAMType

evalParams :: (LLEvaluator Symbol m) => TokenType -> EvalParams -> m ()
evalParams tokenType p = eval tokenType (source p) (asciiLabel p) (stackTypeOptions p) (ramTypeOptions p)

eval :: (LLEvaluator Symbol m) => TokenType -> Source -> Bool -> StackType -> RAMType -> m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: (LLEvaluator Symbol m) => TokenList -> Bool -> StackType -> RAMType -> m ()
evalTL tl ascii st rt = evalTL' =<< liftSafe (parseTL tl ascii) where evalTL' il = evalIL il st rt

evalIL :: (LLEvaluator Symbol m) => InstructionList -> StackType -> RAMType -> m ()
evalIL il s ListRAMType    = evalIL' il s []
evalIL il s SeqRAMType     = evalIL' il s Seq.empty
evalIL il s SListRAMType   = evalIL' il s SList.sListEmpty
evalIL il s MapListRAMType = evalIL' il s MapList.mapListEmpty

evalIL' :: (RLLEvaluator Symbol r m) => InstructionList -> StackType -> r -> m ()
evalIL' il ListStackType  = start il []
evalIL' il SeqStackType   = start il Seq.empty
evalIL' il SListStackType = start il SList.sListEmpty

start :: (SRLLEvaluator Symbol s r m) => InstructionList -> s -> r -> m ()
start il = next (IU (Vector.fromList il) 0 (IS []))

next :: (SRLLEvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
next (IU il ic is) s r = doInstruction' =<< liftSafe (indexSafe il ic) where doInstruction' i = doInstruction i (IU il (ic+1) is) s r

stackNext :: (SRLLEvaluator Symbol s r m) => InstructionUnit -> r -> s -> m ()
stackNext ic r s = next ic s r

iuNext :: (SRLLEvaluator Symbol s r m) => s -> r -> InstructionUnit -> m ()
iuNext s r ic = next ic s r

----

doInstruction :: (SRLLEvaluator Symbol s r m) => Instruction -> InstructionUnit -> s -> r -> m ()

-- | IO instructions
doInstruction  (IOStack op)  iu s r = doIOWS op iu s r

-- | Stack instructions
doInstruction (Stack  op)   iu s r = stackNext iu r =<< liftSafe (stackOp  op    s)

-- | Arithmetic
doInstruction (Binary op)   iu s r = stackNext iu r =<< liftSafe (binaryOp op    s)

-- | Heap access
doInstruction (Memory op)      iu s r = doRamWS op iu s r

-- | Control
doInstruction (Low (Mark     _))  iu                          s r = iuNext s r   iu
doInstruction (Low Return      ) (IU il _  (IS (a:is))) s r = iuNext s r $ IU il a $ IS     is
doInstruction (Low (Call     l)) (IU il ic (IS is)    ) s r = call =<< findAddress il l where call a = iuNext s r $ IU il a $ IS (ic:is)
doInstruction (Low (Jump     l)) (IU il _   is        ) s r = jump =<< findAddress il l where jump a = iuNext s r $ IU il a          is
doInstruction (Low (Branch t l)) (IU il ic is) s r = doBranch =<< liftSafe (pop1 s) where
  doBranch (e , s')
    | isNotJump t e = iuNext s' r $ IU il ic  is
    | otherwise     = branch =<< findAddress il l where branch ic' = iuNext s' r $ IU il ic' is

-- | Other
doInstruction End iu s r = doEnd iu s r
doInstruction i   iu _ _ = liftError $ "Can't do " <> show i <> " " <> show iu

doRamWS :: (SRLLEvaluator Symbol s r m) => MemoryOperator -> InstructionUnit -> s -> r -> m ()
doRamWS Store iu s r = doStore =<< liftSafe (pop2 s) where doStore (v , a , s') = next iu s' $ store a v r
doRamWS Load  iu s r = doLoad  =<< liftSafe (pop1 s) where doLoad  (a , s') = stackNext iu r $ flipPush1 s' $ genericLoad r a
doRamWS _     _  _ _ = error "doRamWS"

-- | IO instructions

doIOWS :: (SRLLEvaluator Symbol s r m) => IOOperator -> InstructionUnit -> s -> r -> m ()
doIOWS OutputChar = doOutputChar
doIOWS InputChar  = doInputChar
doIOWS OutputDec  = doOutputDec
doIOWS InputDec   = doInputDec

doOutputChar :: (SRLLEvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doOutputChar iu s r = doOutputChar' =<< liftSafe (pop1 s) where
  doOutputChar' (e , s') = wPutIntAsChar (fromIntegral e) *> next iu s' r

doInputChar :: (SRLLEvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doInputChar iu s r = doInputChar' =<< liftSafe (pop1 s) where
  doInputChar' (address , s') = doInputChar'' =<< wGetChar where
    doInputChar'' char = next iu s' $ storeChar address char r

doOutputDec :: (SRLLEvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doOutputDec iu s r = doOutputDec' =<< liftSafe (pop1 s) where
  doOutputDec' (e , s') = wPutStr (show e) *> next iu s' r

doInputDec :: (SRLLEvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doInputDec iu s r = doInputDec' =<< liftSafe (pop1 s) where
  doInputDec' (address , s') = doInputDec'' =<< wGetLine where
    doInputDec'' line = next iu s' =<< liftSafe (storeNum address line r)

-- | Terminate instruction
doEnd :: (SRLLEvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doEnd iu s _ = logData s <* logData iu
