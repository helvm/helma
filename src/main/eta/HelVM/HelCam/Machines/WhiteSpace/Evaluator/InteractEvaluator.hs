module HelVM.HelCam.Machines.WhiteSpace.Evaluator.InteractEvaluator (interactEval, batchEvalTL, evalTL, batchEvalIL, evalIL) where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.Machines.WhiteSpace.Instruction
import HelVM.HelCam.Machines.WhiteSpace.Parser
import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.Util

import Data.Char

interactEval :: Bool -> Source -> IO ()
interactEval ascii = interact . evalIL . parse ascii

batchEvalTL :: Bool -> TokenList -> Output
batchEvalTL ascii = batchEvalIL . parseTL ascii

batchEvalIL :: InstructionList -> Output
batchEvalIL = flip evalIL ([]::String)

----

evalTL :: Bool -> TokenList -> Interact
evalTL ascii = evalIL . parseTL ascii

evalIL :: InstructionList -> Interact
evalIL il = next  (IU il 0 (IS [])) (Stack []) (Heap [])

next :: InstructionUnit -> Stack -> Heap -> Interact
next (IU il ic is) = doInstruction (il!!ic) (IU il (ic+1) is)

----

doInstruction :: Instruction -> InstructionUnit -> Stack -> Heap -> Interact
-- IO instructions
doInstruction  OutputChar iu s h = doOutputChar iu s h
doInstruction  InputChar  iu s h = doInputChar  iu s h
doInstruction  OutputNum  iu s h = doOutputNum  iu s h
doInstruction  InputNum   iu s h = doInputNum   iu s h

-- Stack instructions
doInstruction (Const symbol) iu (Stack                 s ) h = next iu (Stack                     (symbol:s)) h
doInstruction (Copy  index)  iu (Stack                 s ) h = next iu (Stack                 ((s!!index):s)) h
doInstruction (Slide index)  iu (Stack         (symbol:s)) h = next iu (Stack          (symbol:drop index s)) h
doInstruction  Dup           iu (Stack         (symbol:s)) h = next iu (Stack              (symbol:symbol:s)) h
doInstruction  Swap          iu (Stack (symbol:symbol':s)) h = next iu (Stack             (symbol':symbol:s)) h
doInstruction  Discard       iu (Stack              (_:s)) h = next iu (Stack                             s ) h

-- Arithmetic
doInstruction (Binary op)    iu (Stack (symbol:symbol':s)) h = next iu (Stack (doBinary op symbol symbol':s)) h

-- Heap access
doInstruction Store iu (Stack (value:address:s)) h = next iu (Stack                 s ) (store address value h)
doInstruction Load  iu (Stack       (address:s)) h = next iu (Stack (load h address:s))                      h

-- Control
doInstruction (Mark     _)  iu s h  = next iu s h
doInstruction  Return      (IU il _  (IS (address:is))) s h = next (IU il  address           (IS is)     ) s h
doInstruction (Call     l) (IU il ic (IS is)          ) s h = next (IU il (findAddress il l) (IS (ic:is))) s h
doInstruction (Jump     l) (IU il _   is              ) s h = next (IU il (findAddress il l)  is         ) s h
doInstruction (Branch t l) (IU il ic is) (Stack (symbol:s)) h
  | doBranchTest t symbol = next (IU il (findAddress il l) is) (Stack s) h
  | otherwise             = next (IU il  ic                is) (Stack s) h

-- Other
doInstruction End _ _ _ = doEnd
doInstruction i   _ _ _ = error $ "Can't do " ++ show i

----

emptyStackError :: Instruction -> Interact
emptyStackError i = error $ "Empty stack for instruction " ++ show i

emptyInputError :: Instruction -> Interact
emptyInputError i = error $ "Empty input for instruction " ++ show i

----

-- IO instructions

doOutputChar :: InstructionUnit -> Stack -> Heap -> Interact
doOutputChar _  (Stack [])        _ input = emptyStackError OutputChar input
doOutputChar iu (Stack (value:s)) h input = chr (fromInteger value) : next iu (Stack s) h input

doOutputNum :: InstructionUnit -> Stack -> Heap -> Interact
doOutputNum _  (Stack [])        _ input = emptyStackError OutputNum input
doOutputNum iu (Stack (value:s)) h input = show value ++ next iu (Stack s) h input

doInputChar :: InstructionUnit -> Stack -> Heap -> Interact
doInputChar _   _                  _       []     = emptyInputError InputChar []
doInputChar _  (Stack [])          _       input  = emptyStackError InputChar input
doInputChar iu (Stack (address:s)) h (char:input) = next iu (Stack s) (store address (toInteger (ord char)) h) input

doInputNum :: InstructionUnit -> Stack -> Heap -> Interact
doInputNum _   _                  _ []    = emptyInputError InputNum []
doInputNum _  (Stack [])          _ input = emptyStackError InputNum input
doInputNum iu (Stack (address:s)) h input = next iu (Stack s) (storeNum address line h) input'
  where (line, input') = splitStringByEndLine input

----

doEnd :: Interact
doEnd _ = []
