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
doInstruction a@(Automaton (_ , Simple MoveR  : _) _) = doInstruction (moveRAutomaton a)
doInstruction a@(Automaton (_ , Simple MoveL  : _) _) = doInstruction (moveLAutomaton a)
doInstruction a@(Automaton (_ , Simple Inc    : _) _) = doInstruction (incAutomaton a)
doInstruction a@(Automaton (_ , Simple Dec    : _) _) = doInstruction (decAutomaton a)
doInstruction a@(Automaton (_ , Simple Output : _) _) = doOutputChar  a
doInstruction a@(Automaton (_ , Simple Input  : _) _) = doInputChar   a
doInstruction a@(Automaton (_ , JmpPast       : _) _) = doJmpPast     a
doInstruction a@(Automaton (_ , JmpBack       : _) _) = doJmpBack     a
doInstruction a@(Automaton (_ , []               ) _) = doEnd         a

-- | Control instruction

doJmpPast :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doJmpPast (Automaton table tape@(_ , 0 : _)) = doInstruction (Automaton (jumpPast table) tape)
doJmpPast (Automaton table tape            ) = doInstruction (Automaton (nextInst table) tape)

doJmpBack :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doJmpBack (Automaton table tape@(_ , 0 : _)) = doInstruction (Automaton (nextInst table) tape)
doJmpBack (Automaton table tape            ) = doInstruction (Automaton (jumpBack table) tape)

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar   (Automaton _ (_ ,    [])) = error "Illegal State"
doOutputChar a@(Automaton _ (_ , e : _)) = wPutChar (toChar e) *> doInstruction (nextInstAutomaton a)

doInputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar a = (doInstruction . newAutomatonForChar a) =<< wGetChar

-- | Terminate instruction
doEnd :: BIO m => Automaton e -> m $ Automaton e
doEnd = pure

-- | Types
--type AutomatonSame e = Same (Automaton e)

newAutomatonForChar :: Symbol e => Automaton e -> Char -> Automaton e
newAutomatonForChar (Automaton table tape) = Automaton (nextInst table) . flip writeSymbol tape

moveRAutomaton :: Symbol e => Automaton e -> Automaton e
moveRAutomaton (Automaton table tape) = (Automaton (nextInst table) (moveHeadRight tape))

moveLAutomaton :: Symbol e => Automaton e -> Automaton e
moveLAutomaton (Automaton table tape) = (Automaton (nextInst table) (moveHeadLeft tape))

incAutomaton :: Symbol e => Automaton e -> Automaton e
incAutomaton (Automaton table tape) = (Automaton (nextInst table) (nextSymbol tape))

decAutomaton :: Symbol e => Automaton e -> Automaton e
decAutomaton (Automaton table tape) = (Automaton (nextInst table) (prevSymbol tape))

nextInstAutomaton :: Automaton e -> Automaton e
nextInstAutomaton (Automaton table tape) = (Automaton (nextInst table) tape)

data Automaton e = Automaton
  { unitTable :: Table
  , unitTape  :: FullTape e
  }
  deriving stock (Eq , Read , Show)
