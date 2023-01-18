module HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Evaluator(
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser
import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions (Table)
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions as Table

import           HelVM.HelMA.Automata.BrainFuck.Common.PureInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols          (FullTape, FullTapeD)
import qualified HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols          as Tape

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra

import           Control.Applicative.Tools
import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> LoopLimit -> DumpType -> m ()
evalSource source tape limit dt = logDump dt =<< run limit (Automaton ([] , tokenize source) tape)

run :: (BIO m , Symbol e) => LoopLimit -> Automaton e -> m $ Automaton e
run = loopMWithLimit nextState

nextState :: (BIO m , Symbol e) => Automaton e -> m $ AutomatonSame e
nextState = tee (flip doInstructionOpt) currentInstruction

doInstructionOpt :: (BIO m , Symbol e) => Maybe FlatInstruction -> Automaton e -> m $ AutomatonSame e
doInstructionOpt (Just i) = Right <.> doInstruction i
doInstructionOpt Nothing  = Left <.> pure

doInstruction :: (BIO m , Symbol e) => FlatInstruction -> Automaton e -> m $ Automaton e
doInstruction  JmpPast           = doJmpPast
doInstruction  JmpBack           = doJmpBack
doInstruction (Simple Output)    = doOutputChar
doInstruction (Simple Input )    = doInputChar
doInstruction (Simple (Pure i )) = pure . nextInstAutomaton . updateTape (doPure i)

-- | Control instruction
doJmpPast :: (MonadSafe m , Symbol e) => Automaton e -> m $ Automaton e
doJmpPast = teeMap (flip doJmpPastForValue) currentSymbolSafe

doJmpPastForValue :: (Eq a, Num a) => a -> Automaton e -> Automaton e
doJmpPastForValue 0 = updateTable Table.jumpPast
doJmpPastForValue _ = nextInstAutomaton

doJmpBack :: (MonadSafe m , Symbol e) => Automaton e -> m $ Automaton e
doJmpBack = teeMap (flip doJmpBackForValue) currentSymbolSafe

doJmpBackForValue :: (Eq a, Num a) => a -> Automaton e -> Automaton e
doJmpBackForValue 0 = nextInstAutomaton
doJmpBackForValue _ = updateTable Table.jumpBack

-- | IO instructions
doInputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar a = newAutomatonForChar a <$> wGetChar

doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar a = wPutSymbol a $> nextInstAutomaton a --FIXME

wPutSymbol :: (BIO m , Symbol e) => Automaton e -> m ()
wPutSymbol = wPutChar . toChar <=< currentSymbolSafe

-- | Pure Instruction
doPure :: Symbol e => PureInstruction -> FullTapeD e
doPure MoveR = Tape.moveHeadRight
doPure MoveL = Tape.moveHeadLeft
doPure Inc   = Tape.nextSymbol
doPure Dec   = Tape.prevSymbol

-- | Accessors

currentSymbolSafe :: MonadSafe m => Automaton e -> m e
currentSymbolSafe = Tape.readSymbolSafe . unitTape

currentInstruction :: Automaton e -> Maybe FlatInstruction
currentInstruction = Table.currentInstruction . unitTable

-- | Constructors

newAutomatonForChar :: Symbol e => Automaton e -> Char -> Automaton e
newAutomatonForChar (Automaton table tape) = Automaton (Table.nextInst table) . flip Tape.writeSymbol tape --FIXME

nextInstAutomaton :: Automaton e -> Automaton e
nextInstAutomaton = updateTable Table.nextInst

updateTable :: (Table -> Table) -> Automaton e -> Automaton e
updateTable f a = a { unitTable = f $ unitTable a }

updateTape :: (FullTape e -> FullTape e) -> Automaton e -> Automaton e
updateTape f a = a { unitTape = f $ unitTape a }

-- | Types
type AutomatonSame e = Same (Automaton e)

data Automaton e = Automaton
  { unitTable :: Table
  , unitTape  :: FullTape e
  }
  deriving stock (Eq , Read , Show)

-- | Extras
teeMap :: Functor f => (t -> a -> b) -> (t -> f a) -> t -> f b
teeMap f2 f1 x = f2 x <$> f1 x
