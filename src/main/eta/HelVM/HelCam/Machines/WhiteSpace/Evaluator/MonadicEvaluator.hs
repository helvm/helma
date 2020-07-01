module HelVM.HelCam.Machines.WhiteSpace.Evaluator.MonadicEvaluator (monadicEvalWS, evalWSTL, evalWSIL) where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.Machines.WhiteSpace.Instruction
import HelVM.HelCam.Machines.WhiteSpace.Parser
import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

import Data.Char

monadicEvalWS :: Bool -> Source -> IO ()
monadicEvalWS ascii = evalWSIL . parseWS ascii

----

evalWSTL :: WrapperIO m => Bool -> TokenList -> m ()
evalWSTL ascii = evalWSIL . parseWSTL ascii

evalWSIL :: WrapperIO m => InstructionList -> m ()
evalWSIL il = next (Stack []) (Heap []) (IC 0 (IS []) il)

next :: WrapperIO m => Stack -> Heap -> InstructionControl -> m ()
next s h (IC ip is il) = doInstruction (il!!ip) s h (IC (ip+1) is il)

----

doInstruction :: WrapperIO m => Instruction -> Stack -> Heap -> InstructionControl -> m ()
doInstruction (Mark _)    = next
-- IO instructions
doInstruction  OutputChar = doOutputChar
doInstruction  InputChar  = doInputChar
doInstruction  OutputNum  = doOutputNum
doInstruction  InputNum   = doInputNum
-- Other
doInstruction i = doInstruction' i

doInstruction' :: WrapperIO m => Instruction -> Stack -> Heap -> InstructionControl -> m ()
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

doInstruction'' :: WrapperIO m => Instruction -> Stack -> Heap -> InstructionControl -> m ()
-- Heap access
doInstruction'' Load  (Stack       (address:s)) h = next (Stack (load h address:s))                      h
doInstruction'' Store (Stack (value:address:s)) h = next (Stack                 s ) (store address value h)
doInstruction'' i s h = doInstruction''' i s h

-- Control
doInstruction''' :: WrapperIO m => Instruction -> Stack -> Heap -> InstructionControl -> m ()
doInstruction'''  Return      s h (IC _  (IS (address:is)) il) = next s h (IC  address           (IS is)      il)
doInstruction''' (Call     l) s h (IC ip (IS is)           il) = next s h (IC (findAddress il l) (IS (ip:is)) il)
doInstruction''' (Jump     l) s h (IC _   is               il) = next s h (IC (findAddress il l)  is          il)
doInstruction''' (Branch t l) (Stack (symbol:s)) h (IC ip is il)
    | doBranchTest t symbol = next (Stack s) h (IC (findAddress il l) is il)
    | otherwise             = next (Stack s) h (IC  ip                is il)
-- Other
doInstruction''' i s h ic = doInstruction'''' i s h ic

doInstruction'''' :: WrapperIO m => Instruction -> Stack -> Heap -> InstructionControl -> m ()
-- Other
doInstruction'''' End _ _ _ = doEnd
doInstruction'''' i   _ _ _ = error $ "Can't do " ++ show i

----

emptyStackError :: Instruction -> r
emptyStackError i = error $ "Empty stack for instruction " ++ show i

----

-- IO instructions

doOutputChar :: WrapperIO m => Stack -> Heap -> InstructionControl -> m ()
doOutputChar (Stack [])        _ _  = emptyStackError OutputChar
doOutputChar (Stack (value:s)) h ic = do
  wPutChar (chr (fromInteger value))
  next (Stack s) h ic

doOutputNum :: WrapperIO m => Stack -> Heap -> InstructionControl -> m ()
doOutputNum (Stack [])        _ _  = emptyStackError OutputNum
doOutputNum (Stack (value:s)) h ic = do
  wPutStr $ show value
  next (Stack s) h ic

doInputChar :: WrapperIO m => Stack -> Heap -> InstructionControl -> m ()
doInputChar (Stack [])          _ _  = emptyStackError InputChar
doInputChar (Stack (address:s)) h ic = do
  char <- wGetChar
  next (Stack s) (store address (toInteger (ord char)) h) ic

doInputNum :: WrapperIO m => Stack -> Heap -> InstructionControl -> m ()
doInputNum (Stack [])          _ _  = emptyStackError InputNum
doInputNum (Stack (address:s)) h ic = do
  line <- wGetLine
  next (Stack s) (storeNum address line h) ic

----

doEnd :: WrapperIO m => m ()
doEnd = return ()
