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

evalWSTL :: Bool -> TokenList -> Interact
evalWSTL ascii = evalWSIL . parseWSTL ascii

evalWSIL :: InstructionList -> Interact
evalWSIL il = next (IC 0 (IS []) il) (DS []) (H [])

----

next ::InstructionControl -> DataStack -> Heap -> Interact
next (IC ip is il) = doInstruction (il!!ip) (IC (ip+1) is il)

doInstruction ::Instruction -> InstructionControl -> DataStack -> Heap -> Interact
doInstruction (Label _)   = next
-- IO instructions
doInstruction  OutputChar = doOutputChar
doInstruction  OutputNum  = doOutputNum
doInstruction  InputChar  = doInputChar
doInstruction  InputNum   = doInputNum
-- Other
doInstruction instruction = doInstruction' instruction

doInstruction' ::Instruction -> InstructionControl -> DataStack -> Heap -> Interact
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

doInstruction'' ::Instruction -> InstructionControl -> DataStack -> Heap -> Interact
-- Heap access
doInstruction'' Load  ic (DS (pointer:ds))       h = next ic (DS ((load pointer h):ds))  h
doInstruction'' Store ic (DS (value:pointer:ds)) h = next ic (DS ds)                    (store value pointer h)
-- Other
doInstruction''  End _ _ _ = doEnd
doInstruction''  i   _ _ _ = error $ "Can't do " ++ show i

emptyStackError :: Instruction -> Interact
emptyStackError i = error $ "Empty stack for instruction " ++ show i

-- IO instructions

doOutputChar :: InstructionControl -> DataStack -> Heap -> Interact
doOutputChar _  (DS [])         _ input = emptyStackError OutputChar input
doOutputChar ic (DS (value:ds)) h input = chr (fromInteger value) : next ic (DS ds) h input

doOutputNum :: InstructionControl -> DataStack -> Heap -> Interact
doOutputNum _  (DS [])         _ input = emptyStackError OutputNum input
doOutputNum ic (DS (value:ds)) h input = show value ++ next ic (DS ds) h input

doInputChar :: InstructionControl -> DataStack -> Heap -> Interact
doInputChar _   _                _       []     = emptyInputError InputChar
doInputChar _  (DS [])           _       input  = emptyStackError InputChar input
doInputChar ic (DS (pointer:ds)) h (char:input) = next ic (DS ds) (store (toInteger (ord char)) pointer h) input

doInputNum :: InstructionControl -> DataStack -> Heap -> Interact
doInputNum _   _                _ []    = emptyInputError InputNum
doInputNum _  (DS [])           _ input = emptyStackError InputNum input
doInputNum ic (DS (pointer:ds)) h input = next ic (DS ds) (storeNum line pointer h) input'
  where (line, input') = splitStringByEndLine input

----

doEnd :: Interact
doEnd _ = []

----

emptyInputError :: Instruction -> Output
emptyInputError i = error $ "Empty input for instruction " ++ show i
