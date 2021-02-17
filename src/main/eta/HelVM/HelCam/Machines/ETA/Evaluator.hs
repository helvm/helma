{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.ETA.Evaluator (
  monadicEval,
  interactEval,
  batchEval,
  eval
) where

import HelVM.HelCam.Machines.ETA.EvaluatorUtil

import HelVM.HelCam.Machines.ETA.Lexer
import HelVM.HelCam.Machines.ETA.Token

import HelVM.HelCam.Common.MockIO
import HelVM.HelCam.Common.OrError
import HelVM.HelCam.Common.Util

import qualified System.IO as IO

class Evaluator r where
  eval :: Source -> r
  eval = evalTL . tokenize

  evalTL :: TokenList -> r
  evalTL il = next (IU il 0) (Stack [])

  next :: InstructionUnit -> Stack -> r
  next iu s = doInstruction t iu' s where (t, iu') = nextIU iu

  doInstruction :: Maybe Token -> InstructionUnit -> Stack -> r
  -- IO instructions
  doInstruction (Just O) iu s = doOutputChar iu s
  doInstruction (Just I) iu s = doInputChar  iu s

  -- Stack instructions
  doInstruction (Just N) iu (Stack s) = next iu' (Stack (symbol:s))
    where (symbol, iu') = parseNumber iu
  doInstruction (Just H) iu (Stack (index:s))
    | index <= 0 = next iu (Stack (genericIndexOrError ("Halibut"::Text, s) s (negate index):s))
    | otherwise  = next iu (Stack (symbol <> tops <> s'')) where
      (tops,s')    = splitAt index s
      (symbol,s'') = splitAt 1     s'

  -- Arithmetic
  doInstruction (Just S) iu (Stack (symbol:symbol':s)) = next iu (Stack                            ((symbol' - symbol):s))
  doInstruction (Just E) iu (Stack (symbol:symbol':s)) = next iu (Stack ((symbol' `mod` symbol):(symbol' `div` symbol):s))

  -- Control
  doInstruction (Just R) iu                            s   = next  iu s
  doInstruction (Just T) iu            (Stack     (_:0:s)) = next  iu (Stack s)
  doInstruction (Just T) _             (Stack     (0:_  )) = doEnd
  doInstruction (Just T)    (IU il _ ) (Stack (label:_:s)) = next (IU il $ findAddress il label) (Stack s)
  doInstruction (Just A) iu@(IU il ic) (Stack          s ) = next  iu  (Stack (nextLabel il ic:s))
  doInstruction Nothing _ _  = doEnd

  -- Other
  doInstruction t s iu = error $ "Can't do token " <> show t <> " "  <> show s <> " " <> show iu

  ----
  emptyStackError :: Token -> r
  emptyStackError t = error $ "Empty stack for instruction " <> show t

  emptyInputError :: Token -> r
  emptyInputError t = error $ "Empty input for token " <> show t

  ----
  doEnd :: r
  doOutputChar :: InstructionUnit -> Stack -> r
  doInputChar  :: InstructionUnit -> Stack -> r

----
interactEval :: Source -> IO ()
interactEval source = IO.interact $ evalTL $ tokenize source

batchEval :: Source -> Output
batchEval = flip eval ([]::String)

instance Evaluator (Input -> Output) where
  doEnd _ = []

  doInputChar _   _         []          = emptyInputError I ([]::Input)
  doInputChar _  (Stack []) _           = emptyStackError I ([]::Input)
  doInputChar iu (Stack s) (char:input) = next iu (Stack (ord char:s)) input

  doOutputChar _  (Stack [])         _     = emptyStackError O ([]::Input)
  doOutputChar iu (Stack (symbol:s)) input = chr symbol : next iu (Stack s) input

----
monadicEval :: Source -> IO ()
monadicEval = evalTL . tokenize

instance Evaluator (IO ()) where
  doEnd = pass

  doInputChar _  (Stack []) = emptyStackError I
  doInputChar iu (Stack s)  = do
    char <- IO.getChar
    next iu (Stack (ord char:s))

  doOutputChar _  (Stack [])        = emptyStackError O
  doOutputChar iu (Stack (value:s)) = do
    IO.putChar (chr value)
    next iu $ Stack s

----
instance Evaluator (MockIO ()) where
  doEnd = pass

  doInputChar _  (Stack []) = emptyStackError I
  doInputChar iu (Stack s)  = do
    char <- mockGetChar
    next iu (Stack (ord char:s))

  doOutputChar _  (Stack [])        = emptyStackError O
  doOutputChar iu (Stack (value:s)) = do
    mockPutChar (chr value)
    next iu $ Stack s
