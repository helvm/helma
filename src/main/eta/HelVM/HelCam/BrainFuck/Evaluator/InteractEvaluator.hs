module HelVM.HelCam.BrainFuck.Evaluator.InteractEvaluator (batchEvalBFInt8, batchEvalBFWord8) where

import HelVM.HelCam.BrainFuck.Symbol
import HelVM.HelCam.BrainFuck.TableOfInstructions
import HelVM.HelCam.BrainFuck.TapeOfSymbols
import HelVM.HelCam.BrainFuck.Token
import HelVM.HelCam.BrainFuck.Lexer

import HelVM.HelCam.Common.Util

import Data.Int
import Data.Word

batchEvalBFInt8 :: Source -> Output
batchEvalBFInt8 = flip evalBFInt8 ([]::String)

batchEvalBFWord8 :: Source -> Output
batchEvalBFWord8  = flip evalBFWord8 ([]::String)

evalBFInt8 :: Source -> Interact
evalBFInt8 = flip evalBF (newTape :: Tape Int8)

evalBFWord8 :: Source -> Interact
evalBFWord8  = flip evalBF (newTape :: Tape Word8)

evalBF :: Symbol s => Source -> Tape s -> Interact
evalBF source = doInstruction ([], tokenizeBF source)

doInstruction :: Symbol s => Table -> Tape s -> Interact
doInstruction table@(_, MoveR   :_) tape = doInstruction    (nextInst table) (moveHeadRight tape)
doInstruction table@(_, MoveL   :_) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
doInstruction table@(_, Inc     :_) tape = doInstruction    (nextInst table)   (wSuccSymbol tape)
doInstruction table@(_, Dec     :_) tape = doInstruction    (nextInst table)   (wPredSymbol tape)
doInstruction table@(_, JmpPast :_) tape = doJmpPast                  table                 tape
doInstruction table@(_, JmpBack :_) tape = doJmpBack                  table                 tape
doInstruction table@(_, Output  :_) tape = doOutput                   table                 tape
doInstruction table@(_, Input   :_) tape = doInput                    table                 tape
doInstruction       (_, []        ) _    = doEnd

doJmpPast :: Symbol s => Table -> Tape s -> Interact
doJmpPast table tape@(_, 0:_) = doInstruction (jumpPast table) tape
doJmpPast table tape          = doInstruction (nextInst table) tape

doJmpBack :: Symbol s => Table -> Tape s -> Interact
doJmpBack table tape@(_, 0:_) = doInstruction (nextInst table) tape
doJmpBack table tape          = doInstruction (jumpBack table) tape

doInput :: Symbol s => Table -> Tape s -> Interact
doInput _     _          []     = error "Empty input"
doInput table tape (char:input) = doInstruction (nextInst table) (writeSymbol char tape) input

doOutput :: Symbol s => Table -> Tape s -> Interact
doOutput _          (_, [])       _     = error "Illegal State"
doOutput table tape@(_, symbol:_) input = toChar symbol : doInstruction (nextInst table) tape input

doEnd :: Interact
doEnd _ = []
