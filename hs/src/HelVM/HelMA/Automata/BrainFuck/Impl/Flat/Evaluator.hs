module HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Evaluator(
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions (Table)
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions as Table

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
doInstruction a = checkOpt $ currentInstruction a where
  checkOpt Nothing  = doEnd         a
  checkOpt (Just i) = check i
  check (Simple MoveR ) = doInstruction $ moveRAutomaton a
  check (Simple MoveL ) = doInstruction $ moveLAutomaton a
  check (Simple Inc   ) = doInstruction $ incAutomaton a
  check (Simple Dec   ) = doInstruction $ decAutomaton a
  check (Simple Output) = doOutputChar  a
  check (Simple Input ) = doInputChar   a
  check  JmpPast        = doInstruction (doJmpPast     a)
  check  JmpBack        = doInstruction (doJmpBack     a)

-- | Control instruction
doJmpPast :: Symbol e => Automaton e -> Automaton e
doJmpPast (Automaton table tape@(_ , 0 : _)) = Automaton (Table.jumpPast table) tape
doJmpPast (Automaton table tape            ) = Automaton (Table.nextInst table) tape

doJmpBack :: Symbol e => Automaton e -> Automaton e
doJmpBack (Automaton table tape@(_ , 0 : _)) = Automaton (Table.nextInst table) tape
doJmpBack (Automaton table tape            ) = Automaton (Table.jumpBack table) tape

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

currentInstruction :: Automaton e -> Maybe FlatInstruction
currentInstruction (Automaton table _) = Table.currentInstruction table

newAutomatonForChar :: Symbol e => Automaton e -> Char -> Automaton e
newAutomatonForChar (Automaton table tape) = Automaton (Table.nextInst table) . flip writeSymbol tape

moveRAutomaton :: Symbol e => Automaton e -> Automaton e
moveRAutomaton (Automaton table tape) = Automaton (Table.nextInst table) (moveHeadRight tape)

moveLAutomaton :: Symbol e => Automaton e -> Automaton e
moveLAutomaton (Automaton table tape) = Automaton (Table.nextInst table) (moveHeadLeft tape)

incAutomaton :: Symbol e => Automaton e -> Automaton e
incAutomaton (Automaton table tape) = Automaton (Table.nextInst table) (nextSymbol tape)

decAutomaton :: Symbol e => Automaton e -> Automaton e
decAutomaton (Automaton table tape) = Automaton (Table.nextInst table) (prevSymbol tape)
--decAutomaton a = nextInstAutomaton $ a { unitTape = prevSymbol $ unitTape a }

nextInstAutomaton :: Automaton e -> Automaton e
nextInstAutomaton a = a { unitTable = Table.nextInst $ unitTable a }

data Automaton e = Automaton
  { unitTable :: Table
  , unitTape  :: FullTape e
  }
  deriving stock (Eq , Read , Show)
