{-# Language FlexibleContexts  #-}
{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.WhiteSpace.Evaluator (
  batchSimpleEvalIL,
  batchSimpleEvalTL,
  simpleEvalIL,
  simpleEvalTL,
  eval,
  evalIL,
  evalTL
) where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.Machines.WhiteSpace.Instruction
import HelVM.HelCam.Machines.WhiteSpace.Lexer
import HelVM.HelCam.Machines.WhiteSpace.Parser
import HelVM.HelCam.Machines.WhiteSpace.StackOfSymbols as Stack
import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.OrError
import HelVM.HelCam.Common.Memories.RAM as RAM
import HelVM.HelCam.Common.Memories.Stack as Stack
import HelVM.HelCam.Common.Types.RAMType
import HelVM.HelCam.Common.Types.StackType
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

import Data.IntMap as IntMap
import Data.Sequence as Seq (fromList)

batchSimpleEvalTL :: TokenList -> Output
batchSimpleEvalTL = flip simpleEvalTL emptyInput

batchSimpleEvalIL :: InstructionList -> Output
batchSimpleEvalIL = flip simpleEvalIL emptyInput

simpleEvalTL :: Evaluator r => TokenList -> r
simpleEvalTL tl = evalTL tl False defaultStackType defaultRAMType

simpleEvalIL :: Evaluator r => InstructionList -> r
simpleEvalIL il = evalIL il defaultStackType defaultRAMType

eval :: Evaluator r => Source -> Bool -> StackType -> RAMType -> r
eval source = evalTL $ tokenize source

evalTL :: Evaluator r => TokenList -> Bool -> StackType -> RAMType -> r
evalTL tl ascii = evalIL $ parseTL tl ascii

evalIL :: Evaluator r => InstructionList -> StackType -> RAMType -> r
evalIL il s ListRAMType   = evalIL' il s ([] :: SymbolList)
evalIL il s SeqRAMType    = evalIL' il s (Seq.fromList [] :: Seq Symbol)
evalIL il s IntMapRAMType = evalIL' il s (IntMap.empty :: IntMap Symbol)

evalIL' :: (RAM Symbol m, Evaluator r) => InstructionList -> StackType -> m -> r
evalIL' il ListStackType = start il ([] :: SymbolList)
evalIL' il SeqStackType  = start il (Seq.fromList [] :: Seq Symbol)

start :: (Stack Symbol s, RAM Symbol m, Evaluator r) => InstructionList -> s -> m -> r
start il = next (IU il 0 (IS []))

class Evaluator r where
  next :: (Stack Symbol s, RAM Symbol m) => InstructionUnit -> s -> m -> r
  next iu@(IU il ic is) = doInstruction (indexOrError ("next"::Text,iu) il ic) (IU il (ic+1) is)

  ----

  doInstruction :: (Stack Symbol s, RAM Symbol m) => Instruction -> InstructionUnit -> s -> m -> r

  -- IO instructions
  doInstruction  OutputChar iu stack h = doOutputChar iu stack h
  doInstruction  InputChar  iu stack h = doInputChar  iu stack h
  doInstruction  OutputNum  iu stack h = doOutputNum  iu stack h
  doInstruction  InputNum   iu stack h = doInputNum   iu stack h

  -- Stack instructions
  doInstruction (Liter symbol) iu stack h = next iu (push1   symbol stack) h
  doInstruction (Copy  index)  iu stack h = next iu (copy    index  stack) h
  doInstruction (Slide index)  iu stack h = next iu (slide   index  stack) h
  doInstruction  Dup           iu stack h = next iu (dup            stack) h
  doInstruction  Swap          iu stack h = next iu (Stack.swap     stack) h
  doInstruction  Discard       iu stack h = next iu (discard        stack) h

  -- Arithmetic
  doInstruction (Binary op)    iu stack h = next iu (binaryOp op stack) h

  -- Heap access
  doInstruction Store iu stack h = next iu stack' (store (address::Symbol) value h) where (value, address, stack') = pop2 stack
  doInstruction Load  iu stack h = next iu (push1 (load h address ::Symbol) stack') h where (address, stack') = pop1 stack

  -- Control
  doInstruction (Mark     _)  iu                          stack h = next  iu                                     stack h
  doInstruction  Return      (IU il _  (IS (address:is))) stack h = next (IU il  address           (IS is)     ) stack h
  doInstruction (Call     l) (IU il ic (IS is)          ) stack h = next (IU il (findAddress il l) (IS (ic:is))) stack h
  doInstruction (Jump     l) (IU il _   is              ) stack h = next (IU il (findAddress il l)  is         ) stack h
  doInstruction (Branch t l) (IU il ic is) stack h
    | doBranchTest t symbol = next (IU il (findAddress il l) is) stack' h
    | otherwise             = next (IU il ic                 is) stack' h
    where (symbol, stack') = pop1 stack

  -- Other
  doInstruction End _ _ _ = doEnd
  doInstruction i   _ _ _ = error $ "Can't do " <> show i

  ----

  emptyInputError :: Instruction -> r
  emptyInputError i = error $ "Empty input for instruction " <> show i

  -- Special
  doEnd :: r

  -- IO instructions
  doOutputChar :: (Stack Symbol s, RAM Symbol m) => InstructionUnit -> s -> m -> r
  doInputChar  :: (Stack Symbol s, RAM Symbol m) => InstructionUnit -> s -> m -> r
  doOutputNum  :: (Stack Symbol s, RAM Symbol m) => InstructionUnit -> s -> m -> r
  doInputNum   :: (Stack Symbol s, RAM Symbol m) => InstructionUnit -> s -> m -> r

----

storeNum :: RAM Symbol m => Symbol -> Input -> m -> m
storeNum address line = store address (readOrError line :: Symbol)

----

instance Evaluator Interact where
  doEnd _ = []

  doInputChar _  _ _       []     = emptyInputError InputChar ([]::Input)
  doInputChar iu stack h (char:input) = next iu stack' (store address (toInteger (ord char)) h) input
    where (address, stack') = pop1 stack

  doInputNum _  _ _ []    = emptyInputError InputNum ([]::Input)
  doInputNum iu stack h input = next iu stack' (storeNum address line h) input'
    where (address, stack') = pop1 stack
          (line, input') = splitStringByEndLine input

  doOutputChar iu stack h input = chr (fromInteger symbol) : next iu stack' h input
    where (symbol, stack') = pop1 stack

  doOutputNum iu stack h input = show (symbol :: Symbol) <> next iu stack' h input
    where (symbol, stack') = pop1 stack

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doInputChar iu stack h = do
    char <- wGetChar
    next iu stack' (store address (toInteger (ord char)) h)
      where (address, stack') = pop1 stack

  doInputNum iu stack h = do
    line <- wGetLine
    next iu stack' (storeNum address line h)
      where (address, stack') = pop1 stack

  doOutputChar iu stack h = do
    wPutChar (chr (fromInteger value))
    next iu stack' h
      where (value, stack') = pop1 stack

  doOutputNum iu stack h = do
    wPutStr (show (symbol::Symbol))
    next iu stack' h
      where (symbol, stack') = pop1 stack
