{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.BrainFuck.Evaluator (
  batchEvalInt8,
  batchEvalWord8,
  evalInt8,
  evalWord8,
  eval
) where

import HelVM.HelCam.Machines.BrainFuck.Symbol
import HelVM.HelCam.Machines.BrainFuck.TableOfInstructions
import HelVM.HelCam.Machines.BrainFuck.TapeOfSymbols
import HelVM.HelCam.Machines.BrainFuck.Token
import HelVM.HelCam.Machines.BrainFuck.Lexer

import HelVM.HelCam.Common.Types.CellType
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

batchEvalInt8 :: Source -> Output
batchEvalInt8 = flip evalInt8 emptyInput

batchEvalWord8 :: Source -> Output
batchEvalWord8  = flip evalWord8 emptyInput

----

evalInt8 :: Evaluator r => Source -> r
evalInt8 = flip eval Int8Type

evalWord8 :: Evaluator r => Source -> r
evalWord8 = flip eval Word8Type

eval :: Evaluator r => Source -> CellType ->  r
eval source Int8Type  = start source (newTape :: FullTape Int8)
eval source Word8Type = start source (newTape :: FullTape Word8)

start :: (Symbol s, Evaluator r) => String -> FullTape s -> r
start source = doInstruction ([], tokenize source)

class Evaluator r where
  doInstruction :: Symbol s => Table -> FullTape s -> r
  doInstruction table@(_, MoveR   :_) tape = doInstruction    (nextInst table) (moveHeadRight tape)
  doInstruction table@(_, MoveL   :_) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
  doInstruction table@(_, Inc     :_) tape = doInstruction    (nextInst table)   (wNextSymbol tape)
  doInstruction table@(_, Dec     :_) tape = doInstruction    (nextInst table)   (wPrevSymbol tape)
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

instance Evaluator Interact where
  doEnd _ = []

  doInputChar _     _          []     = error "Empty input"
  doInputChar table tape (char:input) = doInstruction (nextInst table) (writeSymbol char tape) input

  doOutputChar _          (_, [])       _     = error "Illegal State"
  doOutputChar table tape@(_, symbol:_) input = toChar symbol : doInstruction (nextInst table) tape input

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doInputChar table tape = do
    char <- wGetChar
    doInstruction (nextInst table) $ writeSymbol char tape

  doOutputChar _          (_, [])       = error "Illegal State"
  doOutputChar table tape@(_, symbol:_) = do
    wPutChar $ toChar symbol
    doInstruction (nextInst table) tape
