module HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Evaluator(
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions (Table)
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions as Table

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols          (FullTape)
import qualified HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols          as Tape

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> LoopLimit -> DumpType -> m ()
evalSource source tape _limit dt = logDump dt =<< doInstruction (Automaton ([] , tokenize source) tape)

doInstruction :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInstruction a = checkOpt $ currentInstruction a where
  checkOpt Nothing  = doEnd         a
  checkOpt (Just i) = check i
  check (Simple MoveR ) = doInstruction $ moveRAutomaton a
  check (Simple MoveL ) = doInstruction $ moveLAutomaton a
  check (Simple Inc   ) = doInstruction $ incAutomaton a
  check (Simple Dec   ) = doInstruction $ decAutomaton a
  check (Simple Output) = doOutputChar  a
  check (Simple Input ) = doInstruction =<< doInputChar a
  check  JmpPast        = doInstruction (doJmpPast     a)
  check  JmpBack        = doInstruction (doJmpBack     a)

-- | Control instruction
doJmpPast :: Symbol e => Automaton e -> Automaton e
--doJmpPast (Automaton table tape@(_ , 0 : _)) = Automaton (Table.jumpPast table) tape
--doJmpPast (Automaton table tape            ) = Automaton (Table.nextInst table) tape
doJmpPast a = doJmpPast' $ currentSymbol a where
  doJmpPast' 0 = updateTable Table.jumpPast a
  doJmpPast' _ = nextInstAutomaton a


doJmpBack :: Symbol e => Automaton e -> Automaton e
doJmpBack a = build $ currentSymbol a where
  build 0 = nextInstAutomaton a
  build _ = updateTable Table.jumpBack a

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar   (Automaton _ (_ ,    [])) = error "Illegal State"
doOutputChar a@(Automaton _ (_ , e : _)) = wPutChar (toChar e) *> doInstruction (nextInstAutomaton a)

doInputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar a = newAutomatonForChar a <$> wGetChar

-- | Terminate instruction
doEnd :: BIO m => Automaton e -> m $ Automaton e
doEnd = pure

-- | Types
--type AutomatonSame e = Same (Automaton e)

currentSymbol :: Automaton e -> e
currentSymbol = Tape.readSymbol . unitTape

currentInstruction :: Automaton e -> Maybe FlatInstruction
currentInstruction = Table.currentInstruction . unitTable

newAutomatonForChar :: Symbol e => Automaton e -> Char -> Automaton e
newAutomatonForChar (Automaton table tape) = Automaton (Table.nextInst table) . flip Tape.writeSymbol tape

moveRAutomaton :: Symbol e => Automaton e -> Automaton e
moveRAutomaton = nextInstAutomaton . updateTape Tape.moveHeadRight

moveLAutomaton :: Symbol e => Automaton e -> Automaton e
moveLAutomaton = nextInstAutomaton . updateTape Tape.moveHeadLeft

incAutomaton :: Symbol e => Automaton e -> Automaton e
incAutomaton = nextInstAutomaton . updateTape Tape.nextSymbol

decAutomaton :: Symbol e => Automaton e -> Automaton e
decAutomaton = nextInstAutomaton . updateTape Tape.prevSymbol

nextInstAutomaton :: Automaton e -> Automaton e
nextInstAutomaton = updateTable Table.nextInst

updateTable :: (Table -> Table) -> Automaton e -> Automaton e
updateTable f a = a { unitTable = f $ unitTable a }

updateTape :: (FullTape e1 -> FullTape e2) -> Automaton e1 -> Automaton e2
updateTape f a = a { unitTape = f $ unitTape a }

data Automaton e = Automaton
  { unitTable :: Table
  , unitTape  :: FullTape e
  }
  deriving stock (Eq , Read , Show)
