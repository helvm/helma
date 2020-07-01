module HelVM.HelCam.Machines.BrainFuck.Evaluator.MonadicEvaluator (monadicEvalBF, evalBFInt8, evalBFWord8, evalBF) where

import HelVM.HelCam.Machines.BrainFuck.Symbol
import HelVM.HelCam.Machines.BrainFuck.TableOfInstructions
import HelVM.HelCam.Machines.BrainFuck.TapeOfSymbols
import HelVM.HelCam.Machines.BrainFuck.Token
import HelVM.HelCam.Machines.BrainFuck.Lexer

import HelVM.HelCam.Common.WrapperIO
import HelVM.HelCam.Common.Util

import Data.Int
import Data.Word

monadicEvalBF :: Source -> IO ()
monadicEvalBF = evalBFWord8

evalBFInt8 :: WrapperIO m => Source -> m ()
evalBFInt8 = flip evalBF (newTape :: FullTape Int8)

evalBFWord8 :: WrapperIO m => Source -> m ()
evalBFWord8  = flip evalBF (newTape :: FullTape Word8)

evalBF :: (Symbol s, WrapperIO m) => Source -> FullTape s -> m ()
evalBF source =  doInstruction ([], tokenizeBF source)

--

doInstruction :: (Symbol s, WrapperIO m) => Table -> FullTape s -> m()
doInstruction table@(_, MoveR   :_) tape = doInstruction   (nextInst table) (moveHeadRight tape)
doInstruction table@(_, MoveL   :_) tape = doInstruction   (nextInst table)  (moveHeadLeft tape)
doInstruction table@(_, Inc     :_) tape = doInstruction   (nextInst table)   (wSuccSymbol tape)
doInstruction table@(_, Dec     :_) tape = doInstruction   (nextInst table)   (wPredSymbol tape)
doInstruction table@(_, JmpPast :_) tape = doJmpPast                 table                 tape
doInstruction table@(_, JmpBack :_) tape = doJmpBack                 table                 tape
doInstruction table@(_, Output  :_) tape = doOutput                  table                 tape
doInstruction table@(_, Input   :_) tape = doInput                   table                 tape
doInstruction       (_, []        ) _    = doEnd

doJmpPast :: (Symbol s, WrapperIO m) => Table -> FullTape s -> m()
doJmpPast table tape@(_, 0:_) = doInstruction (jumpPast table) tape
doJmpPast table tape          = doInstruction (nextInst table) tape

doJmpBack :: (Symbol s, WrapperIO m) => Table -> FullTape s -> m()
doJmpBack table tape@(_, 0:_) = doInstruction (nextInst table) tape
doJmpBack table tape          = doInstruction (jumpBack table) tape

doInput :: (Symbol s, WrapperIO m) => Table -> FullTape s -> m()
doInput table tape = do
  char <- wGetChar
  doInstruction (nextInst table) (writeSymbol char tape)

doOutput :: (Symbol s, WrapperIO m) => Table -> FullTape s -> m()
doOutput _          (_, [])       = error "Illegal State"
doOutput table tape@(_, symbol:_) = do
  wPutChar $ toChar symbol
  doInstruction (nextInst table) tape

doEnd :: WrapperIO m => m()
doEnd = return ()
