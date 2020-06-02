module HelVM.HelCam.WhiteSpace.Evaluator.MonadicEvaluator (monadicEvalWS, evalWSTL, evalWSIL) where

import HelVM.HelCam.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.WhiteSpace.Instruction
import HelVM.HelCam.WhiteSpace.Parser
import HelVM.HelCam.WhiteSpace.Token

import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

import Data.Char

monadicEvalWS :: Bool -> Source -> IO ()
monadicEvalWS ascii = evalWSIL . parseWS ascii

evalWSTL :: WrapperIO m => Bool -> TokenList -> m ()
evalWSTL ascii = evalWSIL . parseWSTL ascii

evalWSIL :: WrapperIO m => InstructionList -> m ()
evalWSIL il = next (IC 0 (IS []) il) (DS []) (H [])

----

next :: WrapperIO m => InstructionControl -> DataStack -> Heap -> m ()
next (IC ip is il) = doInstruction (il!!ip) (IC (ip+1) is il)

doInstruction :: WrapperIO m => Instruction -> InstructionControl -> DataStack -> Heap -> m ()
doInstruction (Label _)   = next
-- IO instructions
doInstruction  OutputChar = doOutputChar
doInstruction  OutputNum  = doOutputNum
doInstruction  InputChar  = doInputChar
doInstruction  InputNum   = doInputNum
-- Other
doInstruction instruction = doInstruction' instruction

doInstruction' :: WrapperIO m => Instruction -> InstructionControl -> DataStack -> Heap -> m ()
-- Stack instructions
doInstruction' (Const value) ic (DS               ds ) = next ic (DS                    (value:ds))
doInstruction'  Dup          ic (DS        (value:ds)) = next ic (DS              (value:value:ds))
doInstruction' (Ref   index) ic (DS               ds ) = next ic (DS              ((ds!!index):ds))
doInstruction' (Slide index) ic (DS        (value:ds)) = next ic (DS         (value:drop index ds))
doInstruction'  Swap         ic (DS (value:value':ds)) = next ic (DS             (value':value:ds))
doInstruction'  Discard      ic (DS            (_:ds)) = next ic (DS                           ds )
-- Arithmetic
doInstruction' (Binary op)   ic (DS (value:value':ds)) = next ic (DS (doBinary op value value':ds))
-- Control
doInstruction'  Return         (IC _  (IS (address:is)) il) ds = next (IC  address           (IS is)      il) ds
doInstruction' (Call        l) (IC ip (IS is)           il) ds = next (IC (findAddress il l) (IS (ip:is)) il) ds
doInstruction' (Jump        l) (IC _   is               il) ds = next (IC (findAddress il l)  is          il) ds
doInstruction' (Branch test l) (IC ip is il) (DS (value:ds))
  | doBranchTest test value = next (IC (findAddress il l) is il) (DS ds)
  | otherwise               = next (IC  ip                is il) (DS ds)
-- Other
doInstruction' instruction ic ds = doInstruction'' instruction ic ds

doInstruction'' :: WrapperIO m => Instruction -> InstructionControl -> DataStack -> Heap -> m ()
-- Heap access
doInstruction'' Load  ic (DS (pointer:ds))       h = next ic (DS ((load pointer h):ds))  h
doInstruction'' Store ic (DS (value:pointer:ds)) h = next ic (DS ds)                    (store value pointer h)
-- Other
doInstruction''  End _ _ _ = doEnd
doInstruction''  i   _ _ _ = error $ "Can't do " ++ show i

emptyStackError :: Instruction -> m ()
emptyStackError i = error $ "Empty stack for instruction " ++ show i

-- IO instructions

doOutputChar :: WrapperIO m => InstructionControl -> DataStack -> Heap -> m ()
doOutputChar _  (DS [])         _ = emptyStackError OutputChar
doOutputChar ic (DS (value:ds)) h = do
  wPutChar (chr (fromInteger value))
  next ic (DS ds) h

doOutputNum :: WrapperIO m => InstructionControl -> DataStack -> Heap -> m ()
doOutputNum _  (DS [])         _ = emptyStackError OutputNum
doOutputNum ic (DS (value:ds)) h = do
  wPutStr $ show value
  next ic (DS ds) h

doInputChar :: WrapperIO m => InstructionControl -> DataStack -> Heap -> m ()
doInputChar _  (DS [])           _ = emptyStackError InputChar
doInputChar ic (DS (pointer:ds)) h = do
  char <- wGetChar
  next ic (DS ds) (store (toInteger (ord char)) pointer h)

doInputNum :: WrapperIO m => InstructionControl -> DataStack -> Heap -> m ()
doInputNum _  (DS [])           _ = emptyStackError InputNum
doInputNum ic (DS (pointer:ds)) h = do
  line <- wGetLine
  next ic (DS ds) (storeNum line pointer h)

----

doEnd :: WrapperIO m => m ()
doEnd = return ()
