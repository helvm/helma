module HelVM.HelCam.Machines.ETA.Evaluator.MonadicEvaluator (monadicEval, eval, evalTL) where

import HelVM.HelCam.Machines.ETA.EvaluatorUtil

import HelVM.HelCam.Machines.ETA.Lexer
import HelVM.HelCam.Machines.ETA.Token

import HelVM.HelCam.Common.OrError
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

import Data.Char

monadicEval :: Source -> IO ()
monadicEval = eval

----

eval :: WrapperIO m => Source -> m ()
eval = evalTL . tokenize

evalTL :: WrapperIO m => TokenList -> m ()
evalTL il = do
  next (IU il 0) (Stack [])

next :: WrapperIO m => InstructionUnit -> Stack -> m ()
next iu@(IU il ic) s = do
  wLogStr $ "("  <> toString t <> "," <> show (nextLabel il ic) <>  "," <> show ic <> "," <> show s <>  "),"
  doInstruction t iu' s
    where (t, iu') = nextIU iu

doInstruction :: WrapperIO m => Maybe Token -> InstructionUnit -> Stack -> m ()
-- IO instructions
doInstruction (Just O) iu s = doOutput iu s
doInstruction (Just I) iu s = doInput  iu s

-- Stack instructions
doInstruction (Just N) iu (Stack s) = next iu' (Stack (symbol:s))
  where (symbol, iu') = parseNumber iu
doInstruction (Just H) iu (Stack (index:s))
  | index <= 0 = next iu (Stack (genericIndexOrError ("Halibut", s) s (negate index):s))
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
  
----

doOutput :: WrapperIO m => InstructionUnit -> Stack -> m ()
doOutput _  (Stack       [] ) = emptyStackError O
doOutput iu (Stack (value:s)) = do
  wPutChar (chr (fromIntegral value))
  next iu (Stack s)

doInput :: WrapperIO m => InstructionUnit -> Stack -> m ()
doInput iu (Stack s) = do
  char <- wGetChar
  next iu (Stack (ord char:s))

----

doEnd :: WrapperIO m => m ()
doEnd = return ()
