module HelVM.HelCam.Machines.WhiteSpace.Evaluator.MonadicEvaluator (monadicEval, evalTL, evalIL) where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.Machines.WhiteSpace.Instruction
import HelVM.HelCam.Machines.WhiteSpace.Parser
import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

import Data.Char

monadicEval :: Bool -> Source -> IO ()
monadicEval ascii = evalIL . parse ascii

----

evalTL :: WrapperIO m => Bool -> TokenList -> m ()
evalTL ascii = evalIL . parseTL ascii

evalIL :: WrapperIO m => InstructionList -> m ()
evalIL il = next (IU il 0 (IS [])) (Stack []) (Heap [])

next :: WrapperIO m => InstructionUnit -> Stack -> Heap -> m ()
next (IU il ic is) = doInstruction (il!!ic) (IU il (ic+1) is)

----

doInstruction :: WrapperIO m => Instruction -> InstructionUnit -> Stack -> Heap -> m ()

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
doInstruction (Mark     _) iu s h   = next iu s h
doInstruction  Return      (IU il _  (IS (address:is))) s h = next (IU il  address           (IS is)     ) s h
doInstruction (Call     l) (IU il ic (IS is)          ) s h = next (IU il (findAddress il l) (IS (ic:is))) s h
doInstruction (Jump     l) (IU il _   is              ) s h = next (IU il (findAddress il l)  is         ) s h
doInstruction (Branch t l) (IU il ic is) (Stack (symbol:s)) h
  | doBranchTest t symbol = next (IU il (findAddress il l) is) (Stack s) h
  | otherwise             = next (IU il  ic                is) (Stack s) h

-- Other
doInstruction End _ _ _ = doEnd
doInstruction i   _ _ _ = error $ "Can't do " <> show i

----

emptyStackError :: Instruction -> r
emptyStackError i = error $ "Empty stack for instruction " <> show i

----

-- IO instructions

doOutputChar :: WrapperIO m => InstructionUnit -> Stack -> Heap -> m ()
doOutputChar _  (Stack [])        _ = emptyStackError OutputChar
doOutputChar iu (Stack (value:s)) h = do
  wPutChar $ chr $ fromInteger value
  next iu (Stack s) h

doOutputNum :: WrapperIO m => InstructionUnit -> Stack -> Heap -> m ()
doOutputNum _  (Stack [])        _ = emptyStackError OutputNum
doOutputNum iu (Stack (value:s)) h = do
  wPutStr $ show value
  next iu (Stack s) h

doInputChar :: WrapperIO m => InstructionUnit -> Stack -> Heap -> m ()
doInputChar _  (Stack [])          _ = emptyStackError InputChar
doInputChar iu (Stack (address:s)) h = do
  char <- wGetChar
  next iu (Stack s) $ store address (toInteger (ord char)) h

doInputNum :: WrapperIO m => InstructionUnit -> Stack -> Heap -> m ()
doInputNum _  (Stack [])          _ = emptyStackError InputNum
doInputNum iu (Stack (address:s)) h = do
  line <- wGetLine
  next iu (Stack s) $ storeNum address line h

----

doEnd :: WrapperIO m => m ()
doEnd = return ()
