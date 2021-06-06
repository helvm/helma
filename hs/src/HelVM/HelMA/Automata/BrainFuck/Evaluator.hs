module HelVM.HelMA.Automata.BrainFuck.Evaluator (
  flipUncurryEval,
  uncurryEval,
  evalParams,
  eval
) where

import HelVM.HelMA.Automata.BrainFuck.Symbol
import HelVM.HelMA.Automata.BrainFuck.TableOfInstructions
import HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols
import HelVM.HelMA.Automata.BrainFuck.Token
import HelVM.HelMA.Automata.BrainFuck.Lexer

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.API.TypeOptions
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Types.CellType
import HelVM.HelMA.Common.Util

flipUncurryEval :: Input -> (Source , CellType) -> Output
flipUncurryEval = flip uncurryEval

uncurryEval :: Evaluator r => (Source , CellType) -> r
uncurryEval = uncurry eval

----

evalParams :: Evaluator r => EvalParams ->  r
evalParams p = eval (source p) (cell $ typeOptions p)


eval :: Evaluator r => Source -> CellType ->  r
eval source Int8Type   = start source (newTape :: FullTape Int8)
eval source Word8Type  = start source (newTape :: FullTape Word8)
eval source Int16Type  = start source (newTape :: FullTape Int16)
eval source Word16Type = start source (newTape :: FullTape Word16)
eval source Int32Type  = start source (newTape :: FullTape Int32)
eval source Word32Type = start source (newTape :: FullTape Word32)
eval source Int64Type  = start source (newTape :: FullTape Int64)
eval source Word64Type = start source (newTape :: FullTape Word64)

class Evaluator r where

  start :: Symbol s => String -> FullTape s -> r
  start source = doInstruction ([] , tokenize source)

  doInstruction :: Symbol s => Table -> FullTape s -> r
  doInstruction table@(_ , MoveR   :_) tape = doInstruction    (nextInst table) (moveHeadRight tape)
  doInstruction table@(_ , MoveL   :_) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
  doInstruction table@(_ , Inc     :_) tape = doInstruction    (nextInst table)   (wNextSymbol tape)
  doInstruction table@(_ , Dec     :_) tape = doInstruction    (nextInst table)   (wPrevSymbol tape)
  doInstruction table@(_ , JmpPast :_) tape = doJmpPast                  table                 tape
  doInstruction table@(_ , JmpBack :_) tape = doJmpBack                  table                 tape
  doInstruction table@(_ , Output  :_) tape = doOutputChar               table                 tape
  doInstruction table@(_ , Input   :_) tape = doInputChar                table                 tape
  doInstruction       (_ , []        ) _    = doEnd

  doJmpPast :: Symbol s => Table -> FullTape s -> r
  doJmpPast table tape@(_ , 0:_) = doInstruction (jumpPast table) tape
  doJmpPast table tape          = doInstruction (nextInst table) tape

  doJmpBack :: Symbol s => Table -> FullTape s -> r
  doJmpBack table tape@(_ , 0:_) = doInstruction (nextInst table) tape
  doJmpBack table tape          = doInstruction (jumpBack table) tape

  doEnd        :: r
  doOutputChar :: Symbol s => Table -> FullTape s -> r
  doInputChar  :: Symbol s => Table -> FullTape s -> r

----

instance Evaluator Interact where
  doEnd _ = []

  doInputChar _     tape       []     = error $ "Empty input " <> show tape
  doInputChar table tape (char:input) = doInstruction (nextInst table) (writeSymbol char tape) input

  doOutputChar _     tape@(_ , [])       _     = error $ "Illegal State " <> show tape
  doOutputChar table tape@(_ , symbol:_) input = toChar symbol : doInstruction (nextInst table) tape input

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doInputChar table tape = doInputChar' =<< wGetChar where
    doInputChar' char = doInstruction (nextInst table) $ writeSymbol char tape

  doOutputChar _          (_ , [])       = error "Illegal State"
  doOutputChar table tape@(_ , symbol:_) = wPutChar (toChar symbol) *> doInstruction (nextInst table) tape
