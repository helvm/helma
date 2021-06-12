module HelVM.HelMA.Automata.BrainFuck.Evaluator (
  uncurryEval,
  evalParams,
  eval
) where

import HelVM.HelMA.Automata.BrainFuck.Symbol
import HelVM.HelMA.Automata.BrainFuck.TableOfInstructions
import HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols
import HelVM.HelMA.Automata.BrainFuck.Token
import HelVM.HelMA.Automata.BrainFuck.Lexer

import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.API.TypeOptions
import HelVM.HelMA.Automaton.IO.WrapperIO
import HelVM.HelMA.Automaton.Types.CellType

import HelVM.Common.SafeMonadT

uncurryEval :: Evaluator m_ => (Source , CellType) -> m_
uncurryEval = uncurry eval

----

evalParams :: (Monad m , Evaluator (m ())) => EvalParams -> SafeMonadT_ m
evalParams p = hoistMonad $ eval (source p) (cell $ typeOptions p)

eval :: Evaluator m_ => Source -> CellType ->  m_
eval source Int8Type   = start source (newTape :: FullTape Int8)
eval source Word8Type  = start source (newTape :: FullTape Word8)
eval source Int16Type  = start source (newTape :: FullTape Int16)
eval source Word16Type = start source (newTape :: FullTape Word16)
eval source Int32Type  = start source (newTape :: FullTape Int32)
eval source Word32Type = start source (newTape :: FullTape Word32)
eval source Int64Type  = start source (newTape :: FullTape Int64)
eval source Word64Type = start source (newTape :: FullTape Word64)

class Evaluator m_ where

  start :: Symbol e => Source -> FullTape e -> m_
  start source = doInstruction ([] , tokenize source)

  doInstruction :: Symbol e => Table -> FullTape e -> m_
  doInstruction table@(_ , MoveR   :_) tape = doInstruction    (nextInst table) (moveHeadRight tape)
  doInstruction table@(_ , MoveL   :_) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
  doInstruction table@(_ , Inc     :_) tape = doInstruction    (nextInst table)   (wNextSymbol tape)
  doInstruction table@(_ , Dec     :_) tape = doInstruction    (nextInst table)   (wPrevSymbol tape)
  doInstruction table@(_ , JmpPast :_) tape = doJmpPast                  table                 tape
  doInstruction table@(_ , JmpBack :_) tape = doJmpBack                  table                 tape
  doInstruction table@(_ , Output  :_) tape = doOutputChar               table                 tape
  doInstruction table@(_ , Input   :_) tape = doInputChar                table                 tape
  doInstruction       (_ , []        ) _    = doEnd

  doJmpPast :: Symbol e => Table -> FullTape e -> m_
  doJmpPast table tape@(_ , 0:_) = doInstruction (jumpPast table) tape
  doJmpPast table tape           = doInstruction (nextInst table) tape

  doJmpBack :: Symbol e => Table -> FullTape e -> m_
  doJmpBack table tape@(_ , 0:_) = doInstruction (nextInst table) tape
  doJmpBack table tape           = doInstruction (jumpBack table) tape

  doEnd        :: m_
  doOutputChar :: Symbol e => Table -> FullTape e -> m_
  doInputChar  :: Symbol e => Table -> FullTape e -> m_

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doInputChar table tape = doInputChar' =<< wGetChar where
    doInputChar' char = doInstruction (nextInst table) $ writeSymbol char tape

  doOutputChar _          (_ ,  []) = error "Illegal State"
  doOutputChar table tape@(_ , e:_) = wPutChar (toChar e) *> doInstruction (nextInst table) tape
