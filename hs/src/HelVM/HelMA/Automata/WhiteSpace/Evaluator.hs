module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  flipSimpleEval,
  simpleEval,
  flipSimpleEvalTL,
  simpleEvalTL,
  evalParams,
  eval,
  evalIL,
  evalTL
) where

import HelVM.HelMA.Automata.WhiteSpace.EvaluatorUtil
import HelVM.HelMA.Automata.WhiteSpace.Instruction
import HelVM.HelMA.Automata.WhiteSpace.Lexer
import HelVM.HelMA.Automata.WhiteSpace.Parser
import HelVM.HelMA.Automata.WhiteSpace.StackOfSymbols as Stack
import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.API.TypeOptions

import HelVM.HelMA.Common.IO.WrapperIO

import HelVM.HelMA.Common.Memories.RAM   as RAM
import HelVM.HelMA.Common.Memories.Stack as Stack

import HelVM.HelMA.Common.OrError

import HelVM.HelMA.Common.Types.RAMType
import HelVM.HelMA.Common.Types.StackType
import HelVM.HelMA.Common.Types.TokenType

import HelVM.HelMA.Common.Util

import Data.IntMap   as IntMap
import Data.Sequence as Seq (fromList)

flipSimpleEval :: Input -> (TokenType , Source , Bool , StackType , RAMType) -> Output
flipSimpleEval = flip simpleEval

simpleEval :: Evaluator r => (TokenType , Source , Bool , StackType , RAMType) -> r
simpleEval (tokenType , source , asciiLabel , stackType , ramType) = eval tokenType source asciiLabel stackType ramType

flipSimpleEvalTL :: Input -> TokenList -> Output
flipSimpleEvalTL = flip simpleEvalTL

simpleEvalTL :: Evaluator r => TokenList -> r
simpleEvalTL tl = evalTL tl False defaultStackType defaultRAMType

----

evalParams :: Evaluator r => TokenType ->  EvalParams ->  r
evalParams tokenType p = eval tokenType (source p) (asciiLabel p) (stack $ typeOptions p) (ram $ typeOptions p)

eval :: Evaluator r => TokenType -> Source -> Bool -> StackType -> RAMType -> r
eval tokenType source = evalTL $ tokenize tokenType source

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
  doInstruction Load  iu stack h = next iu (push1 (load h (address::Symbol) ::Symbol) stack') h where (address, stack') = pop1 stack

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
  doInstruction End iu s m = doEnd iu s m
  doInstruction i   iu _ _ = error $ "Can't do " <> show i <> " " <> show iu

  ----

  emptyInputError :: Instruction -> r
  emptyInputError i = error $ "Empty input for instruction " <> show i

  -- Special
  doEnd :: (Stack Symbol s, RAM Symbol m) => InstructionUnit -> s -> m -> r

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
  doEnd _ _ _ _ = []

  doInputChar _  _ _       []     = emptyInputError InputChar ([]::Input)
  doInputChar iu stack h (char:input) = next iu stack' (store (address::Symbol) (toInteger (ord char)) h) input where
    (address, stack') = pop1 stack

  doInputNum _  _ _ []    = emptyInputError InputNum ([]::Input)
  doInputNum iu stack h input = next iu stack' (storeNum address line h) input' where
    (address, stack') = pop1 stack
    (line, input') = splitStringByEndLine input

  doOutputChar iu stack h input = chr (fromInteger symbol) : next iu stack' h input where (symbol, stack') = pop1 stack

  doOutputNum iu stack h input = show (symbol :: Symbol) <> next iu stack' h input where (symbol, stack') = pop1 stack

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd iu stack _ = wLogStrLn (show stack) *> wLogStrLn (show iu) *> pass 
  -- fromList [0,0,-1,0,0,0,0,0,0,3]\n"
  -- fromList [0,0,-1,0,0,0,0,0,3]\n

  doInputChar iu stack h = doInputChar' =<< wGetChar where
    doInputChar' char = next iu stack' (store (address::Symbol) (toInteger (ord char)) h)
    (address, stack') = pop1 stack

  doInputNum iu stack h = doInputNum' =<< wGetLine where
    doInputNum' line = next iu stack' (storeNum address line h)
    (address, stack') = pop1 stack

  doOutputChar iu stack h = wPutChar (chr (fromInteger symbol)) *> next iu stack' h  where (symbol, stack') = pop1 stack

  doOutputNum iu stack h = wPutStr (show (symbol::Symbol)) *> next iu stack' h  where (symbol, stack') = pop1 stack

--  doOutputChar iu stack h = wLogStrLn (">" <> show stack) *> wPutChar (chr (fromInteger symbol)) *> wLogStrLn ("<" <> show stack) *> next iu stack' h  where (symbol, stack') = pop1 stack

--  doOutputNum iu stack h = wLogStrLn (">" <> show stack) *> wPutStr (show (symbol::Symbol)) *> wLogStrLn ("<" <> show stack) *> next iu stack' h   where (symbol, stack') = pop1 stack


--type Evaluated = Text

--newtype Evaluated s m = Evaluated (InstructionUnit , s , m)