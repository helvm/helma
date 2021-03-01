{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.BrainFuck.Evaluator where

import HelVM.HelCam.Machines.BrainFuck.Symbol
import HelVM.HelCam.Machines.BrainFuck.TableOfInstructions
import HelVM.HelCam.Machines.BrainFuck.TapeOfSymbols
import HelVM.HelCam.Machines.BrainFuck.Token
import HelVM.HelCam.Machines.BrainFuck.Lexer

import HelVM.HelCam.Common.MockIO
import HelVM.HelCam.Common.Util

import qualified System.IO as IO

class Evaluator r where
  evalInt8 :: Source -> r
  evalInt8 = flip eval (newTape :: FullTape Int8)

  evalWord8 :: Source -> r
  evalWord8  = flip eval (newTape :: FullTape Word8)

  eval :: Symbol s => Source -> FullTape s -> r
  eval source = doInstruction ([], tokenize source)

  doInstruction :: Symbol s => Table -> FullTape s -> r
  doInstruction table@(_, MoveR   :_) tape = doInstruction    (nextInst table) (moveHeadRight tape)
  doInstruction table@(_, MoveL   :_) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
  doInstruction table@(_, Inc     :_) tape = doInstruction    (nextInst table)   (wSuccSymbol tape)
  doInstruction table@(_, Dec     :_) tape = doInstruction    (nextInst table)   (wPredSymbol tape)
  doInstruction table@(_, JmpPast :_) tape = doJmpPast                  table                 tape
  doInstruction table@(_, JmpBack :_) tape = doJmpBack                  table                 tape
  doInstruction table@(_, Output  :_) tape = doOutputChar               table                 tape
  doInstruction table@(_, Input   :_) tape = doInputChar                table                 tape
  doInstruction       (_, []        ) _    = doEnd

  doJmpPast :: Symbol s => Table -> FullTape s -> r
  doJmpPast table tape@(_, 0:_) = doInstruction (jumpPast table) tape
  doJmpPast table tape          = doInstruction (nextInst table) tape

  doJmpBack :: Symbol s => Table -> FullTape s -> r
  doJmpBack table tape@(_, 0:_) = doInstruction (nextInst table) tape
  doJmpBack table tape          = doInstruction (jumpBack table) tape

  doEnd        :: r
  doOutputChar :: Symbol s => Table -> FullTape s -> r
  doInputChar  :: Symbol s => Table -> FullTape s -> r

----

interactEval :: Source -> IO ()
interactEval source = IO.interact (evalWord8 source)

batchEvalInt8 :: Source -> Output
batchEvalInt8 = flip evalInt8 ([]::String)

batchEvalWord8 :: Source -> Output
batchEvalWord8  = flip evalWord8 ([]::String)

instance Evaluator Interact where
  doEnd _ = []

  doInputChar _     _          []     = error "Empty input"
  doInputChar table tape (char:input) = doInstruction (nextInst table) (writeSymbol char tape) input

  doOutputChar _          (_, [])       _     = error "Illegal State"
  doOutputChar table tape@(_, symbol:_) input = toChar symbol : doInstruction (nextInst table) tape input

----

monadicEval :: Source -> IO ()
monadicEval = evalWord8

instance Evaluator (IO ()) where
  doEnd = pass

  doInputChar table tape = do
    char <- IO.getChar
    doInstruction (nextInst table) (writeSymbol char tape)

  doOutputChar _          (_, [])       = error "Illegal State"
  doOutputChar table tape@(_, symbol:_) = do
    IO.putChar $ toChar symbol
    doInstruction (nextInst table) tape

----

instance Evaluator (MockIO ()) where
  doEnd = pass

  doInputChar table tape = do
    char <- mockGetChar
    doInstruction (nextInst table) (writeSymbol char tape)

  doOutputChar _          (_, [])       = error "Illegal State"
  doOutputChar table tape@(_, symbol:_) = do
    mockPutChar $ toChar symbol
    doInstruction (nextInst table) tape