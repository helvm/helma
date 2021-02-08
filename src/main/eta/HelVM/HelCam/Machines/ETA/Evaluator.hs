{-# Language FlexibleContexts      #-}
{-# Language FlexibleInstances     #-}
module HelVM.HelCam.Machines.ETA.Evaluator (
  batchSimpleEval,
  simpleEval,
  eval
) where

import HelVM.HelCam.Machines.ETA.EvaluatorUtil

import HelVM.HelCam.Machines.ETA.Lexer
import HelVM.HelCam.Machines.ETA.StackOfSymbols as Stack
import HelVM.HelCam.Machines.ETA.Token

import HelVM.HelCam.Common.Memories.Stack as Stack
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.Types.StackType
import HelVM.HelCam.Common.WrapperIO

import Data.Sequence as Seq (fromList)

batchSimpleEval :: Source -> Output
batchSimpleEval = flip simpleEval emptyInput

simpleEval :: Evaluator r => Source -> r
simpleEval source = eval source defaultStackType

----

eval :: Evaluator r => Source -> StackType -> r
eval source = evalTL $ tokenize source

evalTL :: Evaluator r => TokenList -> StackType -> r
evalTL tl ListStackType = start tl ([] :: SymbolList)
evalTL tl SeqStackType  = start tl (Seq.fromList [] :: Seq Symbol)

start :: (Stack Symbol m, Evaluator r) => TokenList -> m -> r
start il = next (IU il 0)

class Evaluator r where
  next :: Stack Symbol m => InstructionUnit -> m -> r
  next iu s = doInstruction t iu' s where (t, iu') = nextIU iu

  doInstruction :: Stack Symbol m => Maybe Token -> InstructionUnit -> m -> r
  -- IO instructions
  doInstruction (Just O) iu s = doOutputChar iu s
  doInstruction (Just I) iu s = doInputChar  iu s

  -- Stack instructions
  doInstruction (Just N) iu s = next iu' (push1 (symbol::Symbol) s) where (symbol, iu') = parseNumber iu
  doInstruction (Just H) iu s = next iu $ halibut s

  -- Arithmetic
  doInstruction (Just S) iu s = next iu $ sub s
  doInstruction (Just E) iu s = next iu $ Stack.divMod s

  -- Control
  doInstruction (Just R) iu s = next iu s
  doInstruction (Just A) iu@(IU il ic) s = next iu (push1 (nextLabel il ic) s)
  doInstruction (Just T) iu@(IU il _ ) s = transfer $ pop2 s where
    transfer (_, 0, s') = next iu s'
    transfer (0, _, _ ) = doEnd
    transfer (l, _, s') = next (IU il $ findAddress il l) s'
  doInstruction Nothing _ _  = doEnd

  ----
  doEnd :: r
  doOutputChar :: Stack Symbol m => InstructionUnit -> m -> r
  doInputChar  :: Stack Symbol m => InstructionUnit -> m -> r

----

emptyInputError :: Evaluator r => Token -> r
emptyInputError t = error $ "Empty input for token " <> show t

----

instance Evaluator Interact where
  doEnd _ = []

  doInputChar _  _       []     = emptyInputError I ([]::Input)
  doInputChar iu s (char:input) = next iu (push1 (ord char) s) input

  doOutputChar iu s input = chr symbol : next iu s' input
    where (symbol, s') = pop1 s

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doInputChar iu s = do
    char <- wGetChar
    next iu $ push1 (ord char) s

  doOutputChar iu s = do
    wPutChar (chr value)
    next iu s'
      where (value, s') = pop1 s
