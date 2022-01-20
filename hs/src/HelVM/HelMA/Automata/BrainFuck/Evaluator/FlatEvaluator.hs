module HelVM.HelMA.Automata.BrainFuck.Evaluator.FlatEvaluator (
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Instruction.FlatInstruction
import           HelVM.HelMA.Automata.BrainFuck.Instruction.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Lexer
import           HelVM.HelMA.Automata.BrainFuck.Symbol
import           HelVM.HelMA.Automata.BrainFuck.TableOfInstructions
import           HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.Common.Control.Logger

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> m ()
evalSource source = doInstruction ([] , tokenize source)

doInstruction :: (BIO m , Symbol e) => Table -> FullTape e -> m ()
doInstruction table@(_ , Simple MoveR  : _) tape = doInstruction    (nextInst table) (moveHeadRight tape)
doInstruction table@(_ , Simple MoveL  : _) tape = doInstruction    (nextInst table)  (moveHeadLeft tape)
doInstruction table@(_ , Simple Inc    : _) tape = doInstruction    (nextInst table)   (wNextSymbol tape)
doInstruction table@(_ , Simple Dec    : _) tape = doInstruction    (nextInst table)   (wPrevSymbol tape)
doInstruction table@(_ , Simple Output : _) tape = doOutputChar               table                 tape
doInstruction table@(_ , Simple Input  : _) tape = doInputChar                table                 tape
doInstruction table@(_ , JmpPast       : _) tape = doJmpPast                  table                 tape
doInstruction table@(_ , JmpBack       : _) tape = doJmpBack                  table                 tape
doInstruction table@(_ , []               ) tape = doEnd table tape

doJmpPast :: (BIO m , Symbol e) => Table -> FullTape e -> m ()
doJmpPast table tape@(_ , 0:_) = doInstruction (jumpPast table) tape
doJmpPast table tape           = doInstruction (nextInst table) tape

doJmpBack :: (BIO m , Symbol e) => Table -> FullTape e -> m ()
doJmpBack table tape@(_ , 0 : _) = doInstruction (nextInst table) tape
doJmpBack table tape             = doInstruction (jumpBack table) tape

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => Table -> FullTape e -> m ()
doOutputChar _          (_ ,    []) = error "Illegal State"
doOutputChar table tape@(_ , e : _) = wPutChar (toChar e) *> doInstruction (nextInst table) tape

doInputChar :: (BIO m , Symbol e) => Table -> FullTape e -> m ()
doInputChar table tape = doInputChar' =<< wGetChar where
  doInputChar' char = doInstruction (nextInst table) $ writeSymbol char tape

-- | Terminate instruction
doEnd :: (BIO m , Show e) => Table -> FullTape e -> m ()
doEnd table tape = logMessageTuple ("table" , show table) *> logMessageTuple ("tape" , show tape)
