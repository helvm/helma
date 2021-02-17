module HelVM.HelCam.Machines.SubLeq.Evaluator.InteractEvaluator (
  interactEval,
  batchEval,
  batchEvalIL
) where

import HelVM.HelCam.Machines.SubLeq.Lexer
import HelVM.HelCam.Machines.SubLeq.Symbol

import HelVM.HelCam.Common.RAM.ListRAM as RAM
import HelVM.HelCam.Common.Util

import qualified System.IO as IO

interactEval :: Source -> IO ()
interactEval = IO.interact . eval

batchEval :: Source -> Output
batchEval source = eval source ([]::String)

batchEvalIL :: SymbolList -> Output
batchEvalIL memory = evalIL memory ([]::String)

type Memory = RAM Symbol

----

eval :: Source -> Interact
eval = evalIL . tokenize

evalIL :: SymbolList -> Interact
evalIL il = doInstruction 0 $ RAM.fromList il

doInstruction :: Symbol -> Memory -> Interact
doInstruction ic m
  | ic < 0    = doEnd
  | src < 0   = doInputChar  dst ic m
  | dst < 0   = doOutputChar src ic m
  | otherwise = doInstruction ic' $ store dst diff m
    where
      src  = load m ic
      dst  = load m $ ic + 1
      diff = load m dst - load m src
      ic'
        | diff <= 0 = load m $ ic + 2
        | otherwise = ic + 3

doEnd :: Interact
doEnd _ = []

doInputChar :: Symbol -> Symbol -> Memory -> Interact
doInputChar _       _  _ []    = error "Empty input"
doInputChar address ic m (value:input) = doInstruction (ic+3) (store address (ord value) m) input

doOutputChar :: Symbol -> Symbol -> Memory -> Interact
doOutputChar address ic memory input = chr (load memory address) : doInstruction (ic+3) memory input
