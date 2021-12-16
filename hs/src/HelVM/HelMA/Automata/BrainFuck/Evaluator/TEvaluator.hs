module HelVM.HelMA.Automata.BrainFuck.Evaluator.TEvaluator (
  uncurryEval,
  evalParams,
  eval
) where

import           HelVM.HelMA.Automata.BrainFuck.Lexer
import           HelVM.HelMA.Automata.BrainFuck.Symbol
import           HelVM.HelMA.Automata.BrainFuck.TableOfInstructions
import           HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols
import           HelVM.HelMA.Automata.BrainFuck.Token

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Types.CellType

uncurryEval :: BusinessIO m => (Source , CellType) -> m ()
uncurryEval = uncurry eval

----

evalParams :: BIO m => EvalParams -> m ()
evalParams p = eval (source p) (cellTypeOptions p)

eval :: BusinessIO m => Source -> CellType -> m ()
eval source Int8Type   = start source (newTape :: FullTape Int8)
eval source Word8Type  = start source (newTape :: FullTape Word8)
eval source Int16Type  = start source (newTape :: FullTape Int16)
eval source Word16Type = start source (newTape :: FullTape Word16)
eval source Int32Type  = start source (newTape :: FullTape Int32)
eval source Word32Type = start source (newTape :: FullTape Word32)
eval source Int64Type  = start source (newTape :: FullTape Int64)
eval source Word64Type = start source (newTape :: FullTape Word64)

start :: (Symbol e , BusinessIO m) => Source -> FullTape e -> m ()
start source = doInstruction ([] , tokenize source)

doInstruction :: (Symbol e , BusinessIO m) => Table -> FullTape e -> m ()
doInstruction table@(_ , MoveR   :_) tape = doInstruction    (nextInst table) (moveHeadRight tape)
doInstruction table@(_ , MoveL   :_) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
doInstruction table@(_ , Inc     :_) tape = doInstruction    (nextInst table)   (wNextSymbol tape)
doInstruction table@(_ , Dec     :_) tape = doInstruction    (nextInst table)   (wPrevSymbol tape)
doInstruction table@(_ , Output  :_) tape = doOutputChar               table                 tape
doInstruction table@(_ , Input   :_) tape = doInputChar                table                 tape
doInstruction table@(_ , JmpPast :_) tape = doJmpPast                  table                 tape
doInstruction table@(_ , JmpBack :_) tape = doJmpBack                  table                 tape
doInstruction table@(_ , []        ) tape = doEnd table tape

doJmpPast :: (Symbol e , BusinessIO m) => Table -> FullTape e -> m ()
doJmpPast table tape@(_ , 0:_) = doInstruction (jumpPast table) tape
doJmpPast table tape           = doInstruction (nextInst table) tape

doJmpBack :: (Symbol e , BusinessIO m) => Table -> FullTape e -> m ()
doJmpBack table tape@(_ , 0 : _) = doInstruction (nextInst table) tape
doJmpBack table tape             = doInstruction (jumpBack table) tape

-- | IO instructions
doOutputChar :: (Symbol e , BusinessIO m) => Table -> FullTape e -> m ()
doOutputChar _          (_ ,    []) = error "Illegal State"
doOutputChar table tape@(_ , e : _) = wPutChar (toChar e) *> doInstruction (nextInst table) tape

doInputChar :: (Symbol e , BusinessIO m) => Table -> FullTape e -> m ()
doInputChar table tape = doInputChar' =<< wGetChar where
  doInputChar' char = doInstruction (nextInst table) $ writeSymbol char tape

-- | Terminate instruction
doEnd :: BusinessIO m => Table -> FullTape e -> m ()
doEnd _ _ = pass
