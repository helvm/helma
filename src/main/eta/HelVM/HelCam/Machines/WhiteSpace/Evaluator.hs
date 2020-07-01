{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.WhiteSpace.Evaluator where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.Machines.WhiteSpace.Instruction
import HelVM.HelCam.Machines.WhiteSpace.Parser
import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.Util

import Data.Char

class Evaluator r where
  evalWSTL :: Bool -> TokenList -> r
  evalWSTL ascii = evalWSIL . parseWSTL ascii

  evalWSIL :: InstructionList -> r
  evalWSIL il = next (Stack []) (Heap []) (IC 0 (IS []) il)

  next :: Stack -> Heap -> InstructionControl -> r
  next s h (IC ip is il) = doInstruction (il!!ip) s h (IC (ip+1) is il)

  ----

  doInstruction :: Instruction -> Stack -> Heap -> InstructionControl -> r
  doInstruction (Mark _)    = next
  -- IO instructions
  doInstruction  OutputChar = doOutputChar
  doInstruction  InputChar  = doInputChar
  doInstruction  OutputNum  = doOutputNum
  doInstruction  InputNum   = doInputNum
  -- Other
  doInstruction i = doInstruction' i

  doInstruction' :: Instruction -> Stack -> Heap -> InstructionControl -> r
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

  doInstruction'' :: Instruction -> Stack -> Heap -> InstructionControl -> r
  -- Heap access
  doInstruction'' Load  (Stack       (address:s)) h = next (Stack (load h address:s))                      h
  doInstruction'' Store (Stack (value:address:s)) h = next (Stack                 s ) (store address value h)
  doInstruction'' i s h = doInstruction''' i s h

  -- Control
  doInstruction''' :: Instruction -> Stack -> Heap -> InstructionControl -> r
  doInstruction'''  Return      s h (IC _  (IS (address:is)) il) = next s h (IC  address           (IS is)      il)
  doInstruction''' (Call     l) s h (IC ip (IS is)           il) = next s h (IC (findAddress il l) (IS (ip:is)) il)
  doInstruction''' (Jump     l) s h (IC _   is               il) = next s h (IC (findAddress il l)  is          il)
  doInstruction''' (Branch t l) (Stack (symbol:s)) h (IC ip is il)
    | doBranchTest t symbol = next (Stack s) h (IC (findAddress il l) is il)
    | otherwise             = next (Stack s) h (IC  ip                is il)
  -- Other
  doInstruction''' i s h ic = doInstruction'''' i s h ic

  doInstruction'''' :: Instruction -> Stack -> Heap -> InstructionControl -> r
  -- Other
  doInstruction'''' End _ _ _ = doEnd
  doInstruction'''' i   _ _ _ = error $ "Can't do " ++ show i

  ----

  emptyStackError :: Instruction -> r
  emptyStackError i = error $ "Empty stack for instruction " ++ show i

  emptyInputError :: Instruction -> r
  emptyInputError i = error $ "Empty input for instruction " ++ show i

  -- IO instructions
  doOutputChar :: Stack -> Heap -> InstructionControl -> r
  doInputChar  :: Stack -> Heap -> InstructionControl -> r
  doOutputNum  :: Stack -> Heap -> InstructionControl -> r
  doInputNum   :: Stack -> Heap -> InstructionControl -> r

  ----
  doEnd :: r

----

interactEvalWS :: Bool -> Source -> IO ()
interactEvalWS ascii source = interact (evalWSIL (parseWS ascii source))

batchEvalWSTL :: Bool -> TokenList -> Output
batchEvalWSTL ascii = batchEvalWSIL . parseWSTL ascii

batchEvalWSIL :: InstructionList -> Output
batchEvalWSIL = flip evalWSIL ([]::String)

instance Evaluator (Input -> Output) where
  doOutputChar (Stack [])         _ _  input = emptyStackError OutputChar input
  doOutputChar (Stack (symbol:s)) h ic input = chr (fromInteger symbol) : next (Stack s) h ic input

  doOutputNum (Stack [])         _ _  input = emptyStackError OutputNum input
  doOutputNum (Stack (symbol:s)) h ic input = show symbol ++ next (Stack s) h ic input

  doInputChar  _                  _ _       []     = emptyInputError InputChar ([]::Input)
  doInputChar (Stack [])          _ _       input  = emptyStackError InputChar input
  doInputChar (Stack (address:s)) h ic (char:input) = next (Stack s) (store address (toInteger (ord char)) h) ic input

  doInputNum  _                  _ _ []    = emptyInputError InputNum ([]::Input)
  doInputNum (Stack [])          _ _ input = emptyStackError InputNum input
  doInputNum (Stack (address:s)) h ic input = next (Stack s) (storeNum address line h) ic input'
    where (line, input') = splitStringByEndLine input

  doEnd _ = []

----

monadicEvalWS :: Bool -> Source -> IO ()
monadicEvalWS ascii = evalWSIL . parseWS ascii

instance Evaluator (IO ()) where
  doOutputChar (Stack [])        _ _  = emptyStackError OutputChar
  doOutputChar (Stack (value:s)) h ic = do
    putChar (chr (fromInteger value))
    next (Stack s) h ic

  doOutputNum (Stack [])        _ _  = emptyStackError OutputNum
  doOutputNum (Stack (value:s)) h ic = do
    putStr (show value)
    next (Stack s) h ic

  doInputChar (Stack [])          _ _  = emptyStackError InputChar
  doInputChar (Stack (address:s)) h ic = do
    char <- getChar
    next (Stack s) (store address (toInteger (ord char)) h) ic

  doInputNum (Stack [])          _ _ = emptyStackError InputNum
  doInputNum (Stack (address:s)) h ic = do
    line <- getLine
    next (Stack s) (storeNum address line h) ic

  doEnd = return ()
