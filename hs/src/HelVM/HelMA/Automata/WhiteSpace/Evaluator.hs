{-#LANGUAGE ConstraintKinds#-}
module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  simpleEval,
  simpleEvalTL,
  evalParams,
  eval,
  evalIL,
  evalTL
) where

import HelVM.HelMA.Automata.WhiteSpace.Addressing
import HelVM.HelMA.Automata.WhiteSpace.Instruction
import HelVM.HelMA.Automata.WhiteSpace.Lexer
import HelVM.HelMA.Automata.WhiteSpace.Parser
import HelVM.HelMA.Automata.WhiteSpace.Symbol
import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.TypeOptions

import HelVM.Common.Containers.Lookup

import HelVM.HelMA.Automaton.IO.WrapperIO

import HelVM.HelMA.Automaton.Memories.RAMConst   as RAM
import HelVM.HelMA.Automaton.Memories.StackConst as Stack

import HelVM.Common.SafeMonadT

import HelVM.HelMA.Automaton.Types.RAMType
import HelVM.HelMA.Automaton.Types.StackType
import HelVM.HelMA.Automaton.Types.TokenType

import HelVM.Common.Util

import Data.Default as Default
import Prelude hiding (swap)

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

simpleEval :: Evaluator Symbol m => (TokenType , Source , Bool , StackType , RAMType) -> SafeMonadT_ m
simpleEval (tokenType , source , asciiLabel , stackType , ramType) = eval tokenType source asciiLabel stackType ramType

simpleEvalTL :: Evaluator Symbol m => TokenList -> SafeMonadT_ m
simpleEvalTL tl = evalTL tl False defaultStackType defaultRAMType

evalParams :: Evaluator Symbol m => TokenType -> EvalParams -> SafeMonadT_ m
evalParams tokenType p = eval tokenType (source p) (asciiLabel p) (stack $ typeOptions p) (ram $ typeOptions p)

eval :: Evaluator Symbol m => TokenType -> Source -> Bool -> StackType -> RAMType -> SafeMonadT_ m
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: Evaluator Symbol m => TokenList -> Bool -> StackType -> RAMType -> SafeMonadT_ m
evalTL tl ascii st rt = evalTL' =<< hoistSafe (parseTL tl ascii) where evalTL' il = evalIL il st rt

evalIL :: Evaluator Symbol m => InstructionList -> StackType -> RAMType -> SafeMonadT_ m
evalIL il s ListRAMType   = evalIL' il s []
evalIL il s SeqRAMType    = evalIL' il s Seq.empty
evalIL il s IntMapRAMType = evalIL' il s IntMap.empty

evalIL' :: Evaluator Symbol m => RAM Symbol r => InstructionList -> StackType -> r -> SafeMonadT_ m
evalIL' il ListStackType = start il []
evalIL' il SeqStackType  = start il Seq.empty

start :: (Stack Symbol s , RAM Symbol r , Evaluator Symbol m) => InstructionList -> s -> r -> SafeMonadT_ m
start il = next (IU il 0 (IS []))

class (Element e , Monad m) => Evaluator e m where

  next :: (Stack e s , RAM e r) => InstructionUnit -> s -> r -> SafeMonadT_ m
  next (IU il ic is) s r = doInstruction' =<< hoistSafe (indexSafe il ic) where doInstruction' i = doInstruction i (IU il (ic+1) is) s r

  stackNext :: (Stack e s , RAM e r) => InstructionUnit -> r -> s -> SafeMonadT_ m
  stackNext ic r s = next ic s r

  iuNext :: (Stack e s , RAM e r) => s -> r -> InstructionUnit -> SafeMonadT_ m
  iuNext s r ic = next ic s r

  ----

  doInstruction :: (Stack e s , RAM e r) => Instruction -> InstructionUnit -> s -> r -> SafeMonadT_ m

  -- IO instructions
  doInstruction  OutputChar iu s r = doOutputChar iu s r
  doInstruction  InputChar  iu s r = doInputChar  iu s r
  doInstruction  OutputNum  iu s r = doOutputNum  iu s r
  doInstruction  InputNum   iu s r = doInputNum   iu s r

  -- Stack instructions
  doInstruction (Liter value) iu s r = stackNext iu r $ genericPush1 value s
  doInstruction (Copy  index) iu s r = stackNext iu r =<< hoistSafe (copy    index  s)
  doInstruction (Slide index) iu s r = stackNext iu r =<< hoistSafe (slide   index  s)
  doInstruction  Dup          iu s r = stackNext iu r =<< hoistSafe (dup            s)
  doInstruction  Swap         iu s r = stackNext iu r =<< hoistSafe (swap           s)
  doInstruction  Discard      iu s r = stackNext iu r =<< hoistSafe (discard        s)

  -- Arithmetic
  doInstruction (Binary op)    iu s r = stackNext iu r =<< hoistSafe (binaryOp op s)

  -- Heap access
  doInstruction Store iu s r = doStore =<< hoistSafe (pop2 s) where doStore (v , a , s') = next iu s' $ store a v r
  doInstruction Load  iu s r = doLoad  =<< hoistSafe (pop1 s) where doLoad  (a , s') = stackNext iu r $ flipPush1 s' $ genericLoad r a

  -- Control
  doInstruction (Mark     _)  iu                          s r = iuNext s r   iu
  doInstruction  Return      (IU il _  (IS (a:is))) s r = iuNext s r $ IU il a $ IS     is
  doInstruction (Call     l) (IU il ic (IS is)    ) s r = iuNext s r $ IU il a $ IS (ic:is) where a = findAddress il l
  doInstruction (Jump     l) (IU il _   is        ) s r = iuNext s r $ IU il a          is  where a = findAddress il l
  doInstruction (Branch t l) (IU il ic is) s r = doBranch =<< hoistSafe (pop1 s) where
    doBranch (e , s')
      | isNotJump t e = iuNext s' r $ IU il ic  is
      | otherwise     = iuNext s' r $ IU il ic' is where ic' = findAddress il l

  -- Other
  doInstruction End iu s r = doEnd iu s r
  doInstruction i   iu _ _ = hoistError $ "Can't do " <> show i <> " " <> show iu

  -- Special
  doEnd :: (Stack e s , RAM e r) => InstructionUnit -> s -> r -> SafeMonadT_ m

  -- IO instructions
  doOutputChar :: (Stack e s , RAM e r) => InstructionUnit -> s -> r -> SafeMonadT_ m
  doInputChar  :: (Stack e s , RAM e r) => InstructionUnit -> s -> r -> SafeMonadT_ m
  doOutputNum  :: (Stack e s , RAM e r) => InstructionUnit -> s -> r -> SafeMonadT_ m
  doInputNum   :: (Stack e s , RAM e r) => InstructionUnit -> s -> r -> SafeMonadT_ m

----

type Element e  = (Default e , Read e , Show e , Integral e)

----

instance (Element e , WrapperIO m) => Evaluator e m where
  doEnd iu s _ = hoistMonad (wLogStrLn (show s) *> wLogStrLn (show iu))

  doInputChar iu s r = doInputChar' =<< hoistSafe (pop1 s) where
    doInputChar' (address , s') = doInputChar'' =<< hoistMonad wGetChar where
      doInputChar'' char = next iu s' $ storeChar address char r

  doInputNum iu s r = doInputNum' =<< hoistSafe (pop1 s) where
    doInputNum' (address , s') = doInputNum'' =<< hoistMonad wGetLine where
      doInputNum'' line = next iu s' =<< hoistSafe (storeNum address line r)

  doOutputChar iu s r = doOutputChar' =<< hoistSafe (pop1 s) where
    doOutputChar' (e , s') = hoistMonad (wPutChar $ genericChr e) *> next iu s' r

  doOutputNum iu s r = doOutputNum' =<< hoistSafe (pop1 s) where
    doOutputNum' (e , s') = hoistMonad (wPutStr $ show e) *> next iu s' r
