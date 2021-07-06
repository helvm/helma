module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  simpleEval,
  simpleEvalTL,
  evalParams,
  eval,
  evalIL,
  evalTL
) where

import HelVM.HelMA.Automata.WhiteSpace.Addressing
import HelVM.HelMA.Automata.WhiteSpace.Instruction
import HelVM.HelMA.Automata.WhiteSpace.Lexer
import HelVM.HelMA.Automata.WhiteSpace.Parser
import HelVM.HelMA.Automata.WhiteSpace.Symbol
import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.TypeOptions

import HelVM.Common.Containers.Lookup

import HelVM.HelMA.Automaton.IO.BusinessIO

import HelVM.HelMA.Automaton.Memories.RAMConst   as RAM
import HelVM.HelMA.Automaton.Memories.StackConst as Stack

import HelVM.Common.Safe

import HelVM.HelMA.Automaton.Types.RAMType
import HelVM.HelMA.Automaton.Types.StackType
import HelVM.HelMA.Automaton.Types.TokenType

import HelVM.Common.Util

import Prelude hiding (swap)

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

simpleEval :: (Evaluator Symbol m) => (TokenType , Source , Bool , StackType , RAMType) -> m ()
simpleEval (tokenType , source , asciiLabel , stackType , ramType) = eval tokenType source asciiLabel stackType ramType

simpleEvalTL :: (Evaluator Symbol m) => TokenList -> m ()
simpleEvalTL tl = evalTL tl False defaultStackType defaultRAMType

evalParams :: (Evaluator Symbol m) => TokenType -> EvalParams -> m ()
evalParams tokenType p = eval tokenType (source p) (asciiLabel p) (stack $ typeOptions p) (ram $ typeOptions p)

eval :: (Evaluator Symbol m) => TokenType -> Source -> Bool -> StackType -> RAMType -> m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: (Evaluator Symbol m) => TokenList -> Bool -> StackType -> RAMType -> m ()
evalTL tl ascii st rt = evalTL' =<< liftSafe (parseTL tl ascii) where evalTL' il = evalIL il st rt

evalIL :: (Evaluator Symbol m) => InstructionList -> StackType -> RAMType -> m ()
evalIL il s ListRAMType   = evalIL' il s []
evalIL il s SeqRAMType    = evalIL' il s Seq.empty
evalIL il s IntMapRAMType = evalIL' il s IntMap.empty

evalIL' :: (REvaluator Symbol r m) => InstructionList -> StackType -> r -> m ()
evalIL' il ListStackType = start il []
evalIL' il SeqStackType  = start il Seq.empty

start :: (SREvaluator Symbol s r m) => InstructionList -> s -> r -> m ()
start il = next (IU il 0 (IS []))

next :: (SREvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
next (IU il ic is) s r = doInstruction' =<< liftSafe (indexSafe il ic) where doInstruction' i = doInstruction i (IU il (ic+1) is) s r

stackNext :: (SREvaluator Symbol s r m) => InstructionUnit -> r -> s -> m ()
stackNext ic r s = next ic s r

iuNext :: (SREvaluator Symbol s r m) => s -> r -> InstructionUnit -> m ()
iuNext s r ic = next ic s r

----

doInstruction :: (SREvaluator Symbol s r m) => Instruction -> InstructionUnit -> s -> r -> m ()

-- IO instructions
doInstruction  OutputChar iu s r = doOutputChar iu s r
doInstruction  InputChar  iu s r = doInputChar  iu s r
doInstruction  OutputNum  iu s r = doOutputNum  iu s r
doInstruction  InputNum   iu s r = doInputNum   iu s r

-- Stack instructions
doInstruction (Liter value) iu s r = stackNext iu r $ genericPush1 value s
doInstruction (Copy  index) iu s r = stackNext iu r =<< liftSafe (copy    index  s)
doInstruction (Slide index) iu s r = stackNext iu r =<< liftSafe (slide   index  s)
doInstruction  Dup          iu s r = stackNext iu r =<< liftSafe (dup            s)
doInstruction  Swap         iu s r = stackNext iu r =<< liftSafe (swap           s)
doInstruction  Discard      iu s r = stackNext iu r =<< liftSafe (discard        s)

-- Arithmetic
doInstruction (Binary op)    iu s r = stackNext iu r =<< liftSafe (binaryOp op s)

-- Heap access
doInstruction Store iu s r = doStore =<< liftSafe (pop2 s) where doStore (v , a , s') = next iu s' $ store a v r
doInstruction Load  iu s r = doLoad  =<< liftSafe (pop1 s) where doLoad  (a , s') = stackNext iu r $ flipPush1 s' $ genericLoad r a

-- Control
doInstruction (Mark     _)  iu                          s r = iuNext s r   iu
doInstruction  Return      (IU il _  (IS (a:is))) s r = iuNext s r $ IU il a $ IS     is
doInstruction (Call     l) (IU il ic (IS is)    ) s r = call =<< findAddress il l where call a = iuNext s r $ IU il a $ IS (ic:is)
doInstruction (Jump     l) (IU il _   is        ) s r = jump =<< findAddress il l where jump a = iuNext s r $ IU il a          is
doInstruction (Branch t l) (IU il ic is) s r = doBranch =<< liftSafe (pop1 s) where
  doBranch (e , s')
    | isNotJump t e = iuNext s' r $ IU il ic  is
    | otherwise     = branch =<< findAddress il l where branch ic' = iuNext s' r $ IU il ic' is

-- Other
doInstruction End iu s r = doEnd iu s r
doInstruction i   iu _ _ = liftError $ "Can't do " <> show i <> " " <> show iu

-- IO instructions
doOutputChar :: (SREvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doOutputChar iu s r = doOutputChar' =<< liftSafe (pop1 s) where
  doOutputChar' (e , s') = wPutChar (genericChr e) *> next iu s' r

doInputChar :: (SREvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doInputChar iu s r = doInputChar' =<< liftSafe (pop1 s) where
  doInputChar' (address , s') = doInputChar'' =<< wGetChar where
    doInputChar'' char = next iu s' $ storeChar address char r

doOutputNum :: (SREvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doOutputNum iu s r = doOutputNum' =<< liftSafe (pop1 s) where
  doOutputNum' (e , s') = wPutStr (show e) *> next iu s' r

doInputNum :: (SREvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doInputNum iu s r = doInputNum' =<< liftSafe (pop1 s) where
  doInputNum' (address , s') = doInputNum'' =<< wGetLine where
    doInputNum'' line = next iu s' =<< liftSafe (storeNum address line r)

-- Terminate instruction
doEnd :: (SREvaluator Symbol s r m) => InstructionUnit -> s -> r -> m ()
doEnd iu s _ = wLogStrLn (show s) *> wLogStrLn (show iu)
