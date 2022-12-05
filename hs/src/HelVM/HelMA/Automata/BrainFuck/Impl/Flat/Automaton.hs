module HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Automaton (
  runSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Types.DumpType

import           Control.Type.Operator

runSource :: (BIO m , Symbol e) => Source -> FullTape e -> DumpType -> m ()
runSource source tape dt = logDump dt =<< doInstruction ([] , tokenize source) tape

doInstruction :: (BIO m , Symbol e) => Table -> FullTape e -> m $ Unit e
doInstruction table@(_ , Simple MoveR  : _) tape = doInstruction (nextInst table) (moveHeadRight tape)
doInstruction table@(_ , Simple MoveL  : _) tape = doInstruction (nextInst table)  (moveHeadLeft tape)
doInstruction table@(_ , Simple Inc    : _) tape = doInstruction (nextInst table)    (nextSymbol tape)
doInstruction table@(_ , Simple Dec    : _) tape = doInstruction (nextInst table)    (prevSymbol tape)
doInstruction table@(_ , Simple Output : _) tape = doOutputChar            table                 tape
doInstruction table@(_ , Simple Input  : _) tape = doInputChar             table                 tape
doInstruction table@(_ , JmpPast       : _) tape = doJmpPast               table                 tape
doInstruction table@(_ , JmpBack       : _) tape = doJmpBack               table                 tape
doInstruction table@(_ , []               ) tape = doEnd                   table                 tape

doJmpPast :: (BIO m , Symbol e) => Table -> FullTape e -> m $ Unit e
doJmpPast table tape@(_ , 0 : _) = doInstruction (jumpPast table) tape
doJmpPast table tape             = doInstruction (nextInst table) tape

doJmpBack :: (BIO m , Symbol e) => Table -> FullTape e -> m $ Unit e
doJmpBack table tape@(_ , 0 : _) = doInstruction (nextInst table) tape
doJmpBack table tape             = doInstruction (jumpBack table) tape

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => Table -> FullTape e -> m $ Unit e
doOutputChar _          (_ ,    []) = error "Illegal State"
doOutputChar table tape@(_ , e : _) = wPutChar (toChar e) *> doInstruction (nextInst table) tape

doInputChar :: (BIO m , Symbol e) => Table -> FullTape e -> m $ Unit e
doInputChar table tape = (doInstruction (nextInst table) . flip writeSymbol tape) =<< wGetChar

-- | Terminate instruction
doEnd :: BIO m => Table -> FullTape e -> m $ Unit e
doEnd table tape = pure $ Unit table tape

-- | Types
data Unit e = Unit
  { unitTable :: Table
  , unitTape  :: FullTape e
  }
  deriving stock (Eq , Read , Show)