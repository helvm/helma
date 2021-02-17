module HelVM.HelCam.Machines.ETA.Evaluator.InteractEvaluator (
  interactEval,
  batchEval
) where

import HelVM.HelCam.Machines.ETA.EvaluatorUtil

import HelVM.HelCam.Machines.ETA.Lexer
import HelVM.HelCam.Machines.ETA.Token

import HelVM.HelCam.Common.OrError
import HelVM.HelCam.Common.Util

import qualified System.IO as IO

interactEval :: Source -> IO ()
interactEval = IO.interact . eval
----

batchEval :: Source -> Output
batchEval = flip eval ([]::String)

eval :: Source -> Interact
eval = evalTL . tokenize

evalTL :: TokenList -> Interact
evalTL il = next (IU il 0) (Stack [])

next :: InstructionUnit -> Stack -> Interact
next iu = doInstruction t iu' where (t, iu') = nextIU iu

doInstruction :: Maybe Token -> InstructionUnit -> Stack -> Interact
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
doEnd :: Interact
doEnd _ = []

doInputChar :: InstructionUnit -> Stack -> Interact
doInputChar _   _         []          = emptyInputError I ([]::Input)
doInputChar iu (Stack s) (char:input) = next iu (Stack (ord char:s)) input

doOutputChar :: InstructionUnit -> Stack -> Interact
doOutputChar _  (Stack       []) _      = emptyStackError O ([]::Input)
doOutputChar iu (Stack (value:s)) input = chr value : next iu (Stack s) input
