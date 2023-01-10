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
evalSource source tape _limit dt = logDump dt =<< doInstruction (Automaton ([] , tokenize source) tape)

doInstruction :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInstruction (Automaton table@(_ , Simple MoveR  : _) tape) = doInstruction (Automaton (nextInst table) (moveHeadRight tape))
doInstruction (Automaton table@(_ , Simple MoveL  : _) tape) = doInstruction (Automaton (nextInst table)  (moveHeadLeft tape))
doInstruction (Automaton table@(_ , Simple Inc    : _) tape) = doInstruction (Automaton (nextInst table)    (nextSymbol tape))
doInstruction (Automaton table@(_ , Simple Dec    : _) tape) = doInstruction (Automaton (nextInst table)    (prevSymbol tape))
doInstruction (Automaton table@(_ , Simple Output : _) tape) = doOutputChar  (Automaton table tape)
doInstruction (Automaton table@(_ , Simple Input  : _) tape) = doInputChar   (Automaton table tape)
doInstruction (Automaton table@(_ , JmpPast       : _) tape) = doJmpPast     (Automaton table tape)
doInstruction (Automaton table@(_ , JmpBack       : _) tape) = doJmpBack     (Automaton table tape)
doInstruction (Automaton table@(_ , []               ) tape) = doEnd         (Automaton table tape)

-- | Control instruction

doJmpPast :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doJmpPast (Automaton table tape@(_ , 0 : _)) = doInstruction (Automaton (jumpPast table) tape)
doJmpPast (Automaton table tape            ) = doInstruction (Automaton (nextInst table) tape)

doJmpBack :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doJmpBack (Automaton table tape@(_ , 0 : _)) = doInstruction (Automaton (nextInst table) tape)
doJmpBack (Automaton table tape            ) = doInstruction (Automaton (jumpBack table) tape)

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar (Automaton _          (_ ,    [])) = error "Illegal State"
doOutputChar (Automaton table tape@(_ , e : _)) = wPutChar (toChar e) *> doInstruction (Automaton (nextInst table) tape)

doInputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar a = (doInstruction . newAutomatonForChar a) =<< wGetChar

-- | Terminate instruction
doEnd :: BIO m => Automaton e -> m $ Automaton e
doEnd = pure

-- | Types
--type AutomatonSame e = Same (Automaton e)

newAutomatonForChar :: Symbol e => Automaton e -> Char -> Automaton e
newAutomatonForChar (Automaton table tape) = Automaton (nextInst table) . flip writeSymbol tape

data Automaton e = Automaton
  { unitTable :: Table
  , unitTape  :: FullTape e
  }
  deriving stock (Eq , Read , Show)
