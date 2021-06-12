module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  simpleEval,
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
import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.API.TypeOptions

import HelVM.HelMA.Common.IO.WrapperIO

import HelVM.HelMA.Common.Memories.RAMConst   as RAM
import HelVM.HelMA.Common.Memories.StackConst as Stack

import HelVM.HelMA.Common.OrError

import HelVM.HelMA.Common.Types.RAMType
import HelVM.HelMA.Common.Types.StackType
import HelVM.HelMA.Common.Types.TokenType

import HelVM.HelMA.Common.Util

import Data.Default as Default

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

simpleEval :: Evaluator Symbol r => (TokenType , Source , Bool , StackType , RAMType) -> r
simpleEval (tokenType , source , asciiLabel , stackType , ramType) = eval tokenType source asciiLabel stackType ramType

simpleEvalTL :: Evaluator Symbol r => TokenList -> r
simpleEvalTL tl = evalTL tl False defaultStackType defaultRAMType

evalParams :: Evaluator Symbol r => TokenType -> EvalParams ->  r
evalParams tokenType p = eval tokenType (source p) (asciiLabel p) (stack $ typeOptions p) (ram $ typeOptions p)

eval :: Evaluator Symbol r => TokenType -> Source -> Bool -> StackType -> RAMType -> r
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: Evaluator Symbol r => TokenList -> Bool -> StackType -> RAMType -> r
evalTL tl ascii = evalIL $ parseTL tl ascii

evalIL :: Evaluator Symbol r => InstructionList -> StackType -> RAMType -> r
evalIL il s ListRAMType   = evalIL' il s []
evalIL il s SeqRAMType    = evalIL' il s Seq.empty
evalIL il s IntMapRAMType = evalIL' il s IntMap.empty

evalIL' :: Evaluator Symbol r => RAM Symbol m => InstructionList -> StackType -> m -> r
evalIL' il ListStackType = start il []
evalIL' il SeqStackType  = start il Seq.empty

class (Default cell , Show cell , Integral cell) => Evaluator cell r where

  start :: (Stack cell s , RAM cell m) => InstructionList -> s -> m -> r
  start il = next (IU il 0 (IS []))

  next :: (Stack cell s , RAM cell m) => InstructionUnit -> s -> m -> r
  next iu@(IU il ic is) = doInstruction (indexOrError ("next"::Text,iu) il ic) (IU il (ic+1) is)

  ----

  doInstruction :: (Stack cell s , RAM cell m) => Instruction -> InstructionUnit -> s -> m -> r

  -- IO instructions
  doInstruction  OutputChar iu stack h = doOutputChar iu stack h
  doInstruction  InputChar  iu stack h = doInputChar  iu stack h
  doInstruction  OutputNum  iu stack h = doOutputNum  iu stack h
  doInstruction  InputNum   iu stack h = doInputNum   iu stack h

  -- Stack instructions
  doInstruction (Liter symbol) iu stack h = next iu (push1   (fromIntegral symbol) stack) h
  doInstruction (Copy  index)  iu stack h = next iu (copy    index  stack) h
  doInstruction (Slide index)  iu stack h = next iu (slide   index  stack) h
  doInstruction  Dup           iu stack h = next iu (dup            stack) h
  doInstruction  Swap          iu stack h = next iu (Stack.swap     stack) h
  doInstruction  Discard       iu stack h = next iu (discard        stack) h

  -- Arithmetic
  doInstruction (Binary op)    iu stack h = next iu (binaryOp op stack) h

  -- Heap access
  doInstruction Store iu stack h = next iu stack' (store address value h) where (value , address , stack') = pop2 stack
  doInstruction Load  iu stack h = next iu (push1 (genericLoad h address) stack') h where (address , stack') = pop1 stack

  -- Control
  doInstruction (Mark     _)  iu                          stack h = next  iu                                     stack h
  doInstruction  Return      (IU il _  (IS (address:is))) stack h = next (IU il  address           (IS is)     ) stack h
  doInstruction (Call     l) (IU il ic (IS is)          ) stack h = next (IU il (findAddress il l) (IS (ic:is))) stack h
  doInstruction (Jump     l) (IU il _   is              ) stack h = next (IU il (findAddress il l)  is         ) stack h
  doInstruction (Branch t l) (IU il ic is) stack h
    | doBranchTest t symbol = next (IU il (findAddress il l) is) stack' h
    | otherwise             = next (IU il ic                 is) stack' h
    where (symbol , stack') = pop1 stack

  -- Other
  doInstruction End iu s m = doEnd iu s m
  doInstruction i   iu _ _ = error $ "Can't do " <> show i <> " " <> show iu

  -- Special
  doEnd :: (Stack cell s , RAM cell m) => InstructionUnit -> s -> m -> r

  -- IO instructions
  doOutputChar :: (Stack cell s , RAM cell m) => InstructionUnit -> s -> m -> r
  doInputChar  :: (Stack cell s , RAM cell m) => InstructionUnit -> s -> m -> r
  doOutputNum  :: (Stack cell s , RAM cell m) => InstructionUnit -> s -> m -> r
  doInputNum   :: (Stack cell s , RAM cell m) => InstructionUnit -> s -> m -> r

----

storeNum :: (Read cell , Integral cell , RAM cell m) => cell -> Input -> m -> m
storeNum address = store address . readOrError

----

instance (Default cell , Read cell , Show cell , Integral cell , WrapperIO m) => Evaluator cell (m ()) where
  doEnd iu stack _ = wLogStrLn (show stack) *> wLogStrLn (show iu)

  doInputChar iu stack h = doInputChar' =<< wGetChar where
    doInputChar' char = next iu stack' (storeChar address char h)
    (address , stack') = pop1 stack

  doInputNum iu stack h = doInputNum' =<< wGetLine where
    doInputNum' line = next iu stack' (storeNum address line h)
    (address , stack') = pop1 stack

  doOutputChar iu stack h = wPutChar (genericChr symbol) *> next iu stack' h  where (symbol , stack') = pop1 stack

  doOutputNum iu stack h = wPutStr (show symbol) *> next iu stack' h  where (symbol , stack') = pop1 stack
