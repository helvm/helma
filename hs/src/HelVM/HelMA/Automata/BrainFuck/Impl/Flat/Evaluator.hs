module HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Evaluator(
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> LoopLimit -> DumpType -> m ()
evalSource source tape _limit dt = logDump dt =<< doInstruction ([] , tokenize source) tape

doInstruction :: (BIO m , Symbol e) => Table -> FullTape e -> m $ Automaton e
doInstruction table@(_ , Simple MoveR  : _) tape = doInstruction (nextInst table) (moveHeadRight tape)
doInstruction table@(_ , Simple MoveL  : _) tape = doInstruction (nextInst table)  (moveHeadLeft tape)
doInstruction table@(_ , Simple Inc    : _) tape = doInstruction (nextInst table)    (nextSymbol tape)
doInstruction table@(_ , Simple Dec    : _) tape = doInstruction (nextInst table)    (prevSymbol tape)
doInstruction table@(_ , Simple Output : _) tape = doOutputChar  (Automaton table tape)
doInstruction table@(_ , Simple Input  : _) tape = doInputChar   (Automaton table tape)
doInstruction table@(_ , JmpPast       : _) tape = doJmpPast     (Automaton table tape)
doInstruction table@(_ , JmpBack       : _) tape = doJmpBack     (Automaton table tape)
doInstruction table@(_ , []               ) tape = doEnd         (Automaton table tape)

-- | Control instruction

doJmpPast :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doJmpPast (Automaton table tape@(_ , 0 : _)) = doInstruction (jumpPast table) tape
doJmpPast (Automaton table tape            ) = doInstruction (nextInst table) tape

doJmpBack :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doJmpBack (Automaton table tape@(_ , 0 : _)) = doInstruction (nextInst table) tape
doJmpBack (Automaton table tape            ) = doInstruction (jumpBack table) tape

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar (Automaton _          (_ ,    [])) = error "Illegal State"
doOutputChar (Automaton table tape@(_ , e : _)) = wPutChar (toChar e) *> doInstruction (nextInst table) tape

doInputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar (Automaton table tape) = (doInstruction (nextInst table) . flip writeSymbol tape) =<< wGetChar

-- | Terminate instruction
doEnd :: BIO m => Automaton e -> m $ Automaton e
doEnd = pure

-- | Types
--type AutomatonSame e = Same (Automaton e)

data Automaton e = Automaton
  { unitTable :: Table
  , unitTape  :: FullTape e
  }
  deriving stock (Eq , Read , Show)
