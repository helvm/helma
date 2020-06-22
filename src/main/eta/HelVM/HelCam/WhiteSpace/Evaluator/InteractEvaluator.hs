module HelVM.HelCam.WhiteSpace.Evaluator.InteractEvaluator (interactEvalWS, batchEvalWSTL, evalWSTL, batchEvalWSIL, evalWSIL) where

import HelVM.HelCam.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.WhiteSpace.Instruction
import HelVM.HelCam.WhiteSpace.Parser
import HelVM.HelCam.WhiteSpace.Token

import HelVM.HelCam.Common.Util

import Data.Char

interactEvalWS :: Bool -> Source -> IO ()
interactEvalWS ascii = interact . evalWSIL . parseWS ascii

batchEvalWSTL :: Bool -> TokenList -> Output
batchEvalWSTL ascii = batchEvalWSIL . parseWSTL ascii

batchEvalWSIL :: InstructionList -> Output
batchEvalWSIL = flip evalWSIL ([]::String)

----

evalWSTL :: Bool -> TokenList -> Interact
evalWSTL ascii = evalWSIL . parseWSTL ascii

evalWSIL :: InstructionList -> Interact
evalWSIL il = next (Stack []) (Heap []) (IC 0 (IS []) il)

next :: Stack -> Heap -> InstructionControl -> Interact
next s h (IC ip is il) = doInstruction (il!!ip) s h (IC (ip+1) is il)

----

doInstruction :: Instruction -> Stack -> Heap -> InstructionControl -> Interact
doInstruction (Mark _)    = next
-- IO instructions
doInstruction  OutputChar = doOutputChar
doInstruction  InputChar  = doInputChar
doInstruction  OutputNum  = doOutputNum
doInstruction  InputNum   = doInputNum
-- Other
doInstruction i = doInstruction' i

doInstruction' :: Instruction -> Stack -> Heap -> InstructionControl -> Interact
-- Stack instructions
doInstruction' (Const symbol) (Stack                 s ) = next (Stack                     (symbol:s))
doInstruction' (Copy  index)  (Stack                 s ) = next (Stack                 ((s!!index):s))
doInstruction' (Slide index)  (Stack         (symbol:s)) = next (Stack          (symbol:drop index s))
doInstruction'  Dup           (Stack         (symbol:s)) = next (Stack              (symbol:symbol:s))
doInstruction'  Swap          (Stack (symbol:symbol':s)) = next (Stack             (symbol':symbol:s))
doInstruction'  Discard       (Stack              (_:s)) = next (Stack                             s )
-- Arithmetic
doInstruction' (Binary op)    (Stack (symbol:symbol':s)) = next (Stack (doBinary op symbol symbol':s))
doInstruction' i s = doInstruction'' i s

doInstruction'' :: Instruction -> Stack -> Heap -> InstructionControl -> Interact
-- Heap access
doInstruction'' Load  (Stack       (address:s)) h = next (Stack (load h address:s))                      h
doInstruction'' Store (Stack (value:address:s)) h = next (Stack                 s ) (store address value h)
doInstruction'' i s h = doInstruction''' i s h

-- Control
doInstruction''' :: Instruction -> Stack -> Heap -> InstructionControl -> Interact
doInstruction'''  Return      s h (IC _  (IS (address:is)) il) = next s h (IC  address           (IS is)      il)
doInstruction''' (Call     l) s h (IC ip (IS is)           il) = next s h (IC (findAddress il l) (IS (ip:is)) il)
doInstruction''' (Jump     l) s h (IC _   is               il) = next s h (IC (findAddress il l)  is          il)
doInstruction''' (Branch t l) (Stack (symbol:s)) h (IC ip is il)
    | doBranchTest t symbol = next (Stack s) h (IC (findAddress il l) is il)
    | otherwise             = next (Stack s) h (IC  ip                is il)
-- Other
doInstruction''' i s h ic = doInstruction'''' i s h ic

doInstruction'''' :: Instruction -> Stack -> Heap -> InstructionControl -> Interact
-- Other
doInstruction'''' End _ _ _ = doEnd
doInstruction'''' i   _ _ _ = error $ "Can't do " ++ show i

----

emptyStackError :: Instruction -> Interact
emptyStackError i = error $ "Empty stack for instruction " ++ show i

emptyInputError :: Instruction -> Interact
emptyInputError i = error $ "Empty input for instruction " ++ show i

----

-- IO instructions

doOutputChar :: Stack -> Heap -> InstructionControl -> Interact
doOutputChar (Stack [])        _ _  input = emptyStackError OutputChar input
doOutputChar (Stack (value:s)) h ic input = chr (fromInteger value) : next (Stack s) h ic input

doOutputNum :: Stack -> Heap -> InstructionControl -> Interact
doOutputNum (Stack [])        _ _  input = emptyStackError OutputNum input
doOutputNum (Stack (value:s)) h ic input = show value ++ next (Stack s) h ic input

doInputChar :: Stack -> Heap -> InstructionControl -> Interact
doInputChar  _                  _ _        []     = emptyInputError InputChar []
doInputChar (Stack [])          _ _        input  = emptyStackError InputChar input
doInputChar (Stack (address:s)) h ic (char:input) = next (Stack s) (store address (toInteger (ord char)) h) ic input

doInputNum :: Stack -> Heap -> InstructionControl -> Interact
doInputNum _                   _ _  []    = emptyInputError InputNum []
doInputNum (Stack [])          _ _  input = emptyStackError InputNum input
doInputNum (Stack (address:s)) h ic input = next (Stack s) (storeNum address line h) ic input'
  where (line, input') = splitStringByEndLine input

----

doEnd :: Interact
doEnd _ = []
