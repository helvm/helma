module HelVM.HelMA.Automata.ETA.Evaluator (
  batchUncurryEval,
  flipUncurryEval,
  uncurryEval,
  evalParams,
  eval
) where

import HelVM.HelMA.Automata.ETA.EvaluatorUtil

import HelVM.HelMA.Automata.ETA.Lexer
import HelVM.HelMA.Automata.ETA.StackOfSymbols as Stack
import HelVM.HelMA.Automata.ETA.Token

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.API.TypeOptions
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Memories.Stack as Stack
import HelVM.HelMA.Common.Util
import HelVM.HelMA.Common.Types.StackType

import Data.Sequence as Seq (fromList)

batchUncurryEval :: (Source , StackType) -> Output
batchUncurryEval = flipUncurryEval emptyInput

flipUncurryEval :: Input -> (Source , StackType) -> Output
flipUncurryEval = flip uncurryEval

uncurryEval :: Evaluator r => (Source , StackType) -> r
uncurryEval = uncurry eval

----

evalParams :: Evaluator r => EvalParams ->  r
evalParams p = eval (source p) (stack $ typeOptions p)

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
    transfer (0, _, _ ) = doEnd iu s
    transfer (l, _, s') = next (IU il $ findAddress il l) s'
  doInstruction Nothing iu s  = doEnd iu s

  ----
  doEnd :: Stack Symbol m => InstructionUnit -> m -> r
  doOutputChar :: Stack Symbol m => InstructionUnit -> m -> r
  doInputChar  :: Stack Symbol m => InstructionUnit -> m -> r

----

emptyInputError :: Token -> r
emptyInputError t = error $ "Empty input for token " <> show t

----

instance Evaluator Interact where
  doEnd _ _ _ = []

  doInputChar _  _       []     = emptyInputError I ([]::Input)
  doInputChar iu s (char:input) = next iu (push1 (ord char) s) input

  doOutputChar iu s input = chr symbol : next iu s' input where (symbol, s') = pop1 s

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd iu s = wLogShow iu *> wLogShow s

  doInputChar iu s = doInputChar' =<< wGetChar where
    doInputChar' char = next iu $ push1 (ord char) s

  doOutputChar iu s = wPutChar (chr symbol) *> next iu s' where (symbol, s') = pop1 s
