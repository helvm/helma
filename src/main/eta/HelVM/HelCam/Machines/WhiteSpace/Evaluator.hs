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
import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.OrError
import HelVM.HelCam.Common.RAM as RAM
import HelVM.HelCam.Common.Types.RAMType
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

import Data.IntMap as IntMap
import Data.Sequence as Seq (fromList)

batchSimpleEvalTL :: TokenList -> Output
batchSimpleEvalTL = flip simpleEvalTL emptyInput

batchSimpleEvalIL :: InstructionList -> Output
batchSimpleEvalIL = flip simpleEvalIL emptyInput

simpleEvalTL :: Evaluator r => TokenList -> r
simpleEvalTL tl = evalTL tl False defaultRAMType

simpleEvalIL :: Evaluator r => InstructionList -> r
simpleEvalIL = flip evalIL defaultRAMType

eval :: Evaluator r => Source -> Bool -> RAMType -> r
eval source = evalTL $ tokenize source

evalTL :: Evaluator r => TokenList -> Bool -> RAMType -> r
evalTL tl ascii = evalIL $ parseTL tl ascii

evalIL :: Evaluator r => InstructionList -> RAMType -> r
evalIL il ListRAMType   = start il ([] :: SymbolList)
evalIL il SeqRAMType    = start il (Seq.fromList [] :: Seq Symbol)
evalIL il IntMapRAMType = start il (IntMap.empty :: IntMap Symbol)

start :: (RAM Symbol m, Evaluator r) => InstructionList -> m -> r
start il = next (IU il 0 (IS [])) (Stack []) 

class Evaluator r where
  next :: RAM Symbol m => InstructionUnit -> Stack -> m -> r
  next iu@(IU il ic is) = doInstruction (genericIndexOrError ("next"::Text,iu) il ic) (IU il (ic+1) is)

  ----

  doInstruction :: RAM Symbol m => Instruction -> InstructionUnit -> Stack -> m -> r

  -- IO instructions
  doInstruction  OutputChar iu s h = doOutputChar iu s h
  doInstruction  InputChar  iu s h = doInputChar  iu s h
  doInstruction  OutputNum  iu s h = doOutputNum  iu s h
  doInstruction  InputNum   iu s h = doInputNum   iu s h

  -- Stack instructions
  doInstruction (Liter symbol) iu (Stack                 s ) h = next iu (Stack                     (symbol:s)) h
  doInstruction (Copy  index)  iu (Stack                 s ) h = next iu (Stack             (( s !!! index):s)) h
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
  doInstruction (Mark     _)  iu                          s h = next  iu                                     s h
  doInstruction  Return      (IU il _  (IS (address:is))) s h = next (IU il  address           (IS is)     ) s h
  doInstruction (Call     l) (IU il ic (IS is)          ) s h = next (IU il (findAddress il l) (IS (ic:is))) s h
  doInstruction (Jump     l) (IU il _   is              ) s h = next (IU il (findAddress il l)  is         ) s h
  doInstruction (Branch t l) (IU il ic is) (Stack (symbol:s)) h
    | doBranchTest t symbol = next (IU il (findAddress il l) is) (Stack s) h
    | otherwise             = next (IU il ic                 is) (Stack s) h

  -- Other
  doInstruction End _ _ _ = doEnd
  doInstruction i   _ _ _ = error $ "Can't do " <> show i

  ----

  emptyStackError :: Instruction -> r
  emptyStackError i = error $ "Empty stack for instruction " <> show i

  emptyInputError :: Instruction -> r
  emptyInputError i = error $ "Empty input for instruction " <> show i

  ----
  doEnd :: r

  -- IO instructions
  doOutputChar :: RAM Symbol m => InstructionUnit -> Stack -> m -> r
  doInputChar  :: RAM Symbol m => InstructionUnit -> Stack -> m -> r
  doOutputNum  :: RAM Symbol m => InstructionUnit -> Stack -> m -> r
  doInputNum   :: RAM Symbol m => InstructionUnit -> Stack -> m -> r

----

storeNum :: RAM Symbol m => Symbol -> Input -> m -> m
storeNum address line = store address (readOrError line :: Symbol)

----


instance Evaluator Interact where
  doEnd _ = []

  doOutputChar _  (Stack [])         _ input = emptyStackError OutputChar input
  doOutputChar iu (Stack (symbol:s)) h input = chr (fromInteger symbol) : next iu (Stack s) h input

  doOutputNum _  (Stack [])         _ input = emptyStackError OutputNum input
  doOutputNum iu (Stack (symbol:s)) h input = show symbol <> next iu (Stack s) h input

  doInputChar _                  _   _       []     = emptyInputError InputChar ([]::Input)
  doInputChar _  (Stack [])          _       input  = emptyStackError InputChar input
  doInputChar iu (Stack (address:s)) h (char:input) = next iu (Stack s) (store address (toInteger (ord char)) h) input

  doInputNum _   _                  _ []    = emptyInputError InputNum ([]::Input)
  doInputNum _  (Stack [])          _ input = emptyStackError InputNum input
  doInputNum iu (Stack (address:s)) h input = next iu (Stack s) (storeNum address line h) input'
    where (line, input') = splitStringByEndLine input

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doOutputChar _  (Stack [])        _ = emptyStackError OutputChar
  doOutputChar iu (Stack (value:s)) h = do
    wPutChar $ chr $ fromInteger value
    next iu (Stack s) h

  doOutputNum _  (Stack [])        _ = emptyStackError OutputNum
  doOutputNum iu (Stack (value:s)) h = do
    wPutStr $ show value
    next iu (Stack s) h

  doInputChar _  (Stack [])          _ = emptyStackError InputChar
  doInputChar iu (Stack (address:s)) h = do
    char <- wGetChar
    next iu (Stack s) (store address (toInteger (ord char)) h)

  doInputNum _  (Stack [])          _ = emptyStackError InputNum
  doInputNum iu (Stack (address:s)) h = do
    line <- wGetLine
    next iu (Stack s) $ storeNum address line h
