module HelVM.HelMA.Automata.ETA.Evaluator (
  batchUncurryEval,
  flipUncurryEval,
  uncurryEval,
  evalParams,
  eval
) where

import HelVM.HelMA.Automata.ETA.EvaluatorUtil

import HelVM.HelMA.Automata.ETA.Lexer
import HelVM.HelMA.Automata.ETA.Token

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.API.TypeOptions
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Memories.StackConst as Stack
import HelVM.HelMA.Common.Util
import HelVM.HelMA.Common.Types.StackType

import Data.Default as Default

import qualified Data.Sequence as Seq

batchUncurryEval :: (Source , StackType) -> Output
batchUncurryEval = flipUncurryEval emptyInput

flipUncurryEval :: Input -> (Source , StackType) -> Output
flipUncurryEval = flip uncurryEval

uncurryEval :: Evaluator Symbol r => (Source , StackType) -> r
uncurryEval = uncurry eval

----

evalParams :: Evaluator Symbol r => EvalParams ->  r
evalParams p = eval (source p) (stack $ typeOptions p)

eval :: Evaluator Symbol r => Source -> StackType -> r
eval source = evalTL $ tokenize source

evalTL :: Evaluator Symbol r => TokenList -> StackType -> r
evalTL tl ListStackType = start tl []
evalTL tl SeqStackType  = start tl Seq.empty

start :: Evaluator Symbol r => Stack Symbol m => TokenList -> m -> r
start il = next (IU il 0)

class (Show cell , Integral cell) => Evaluator cell r where

  next :: Stack cell m => InstructionUnit -> m -> r
  next iu s = doInstruction t iu' s where (t , iu') = nextIU iu

  doInstruction :: Stack cell m => Maybe Token -> InstructionUnit -> m -> r
  -- IO instructions
  doInstruction (Just O) iu s = doOutputChar iu s
  doInstruction (Just I) iu s = doInputChar  iu s

  -- Stack instructions
  doInstruction (Just N) iu s = next iu' (push1 symbol s) where (symbol , iu') = parseNumber iu
  doInstruction (Just H) iu s = next iu $ halibut s

  -- Arithmetic
  doInstruction (Just S) iu s = next iu $ sub s
  doInstruction (Just E) iu s = next iu $ Stack.divMod s

  -- Control
  doInstruction (Just R) iu s = next iu s
  doInstruction (Just A) iu@(IU il ic) s = next iu (push1 (genericNextLabel il ic) s)
  doInstruction (Just T) iu@(IU il _ ) s = transfer $ pop2 s where
    transfer (_ , 0 , s') = next iu s'
    transfer (0 , _ , _ ) = doEnd iu s
    transfer (l , _ , s') = next (IU il $ genericFindAddress il l) s'
  doInstruction Nothing iu s  = doEnd iu s

  ----
  doEnd :: Stack cell m => InstructionUnit -> m -> r
  doOutputChar :: Stack cell m => InstructionUnit -> m -> r
  doInputChar  :: Stack cell m => InstructionUnit -> m -> r

----

emptyInputError :: Token -> r
emptyInputError t = error $ "Empty input for token " <> show t

----

instance (Default cell , Read cell , Show cell , Integral cell) => Evaluator cell Interact where
  doEnd _ _ _ = []

  doInputChar _  _       []     = emptyInputError I ([]::Input)
  doInputChar iu s (char:input) = next iu (pushChar1 char s) input

  doOutputChar iu s input = genericChr symbol : next iu s' input where (symbol , s') = pop1 s

----

instance (Default cell , Read cell , Show cell , Integral cell , WrapperIO m) => Evaluator cell (m ()) where
  doEnd iu s = wLogShow iu *> wLogShow s

  doInputChar iu s = doInputChar' =<< wGetChar where
    doInputChar' char = next iu $ pushChar1 char s

  doOutputChar iu s = wPutChar (genericChr symbol) *> next iu s' where (symbol , s') = pop1 s
