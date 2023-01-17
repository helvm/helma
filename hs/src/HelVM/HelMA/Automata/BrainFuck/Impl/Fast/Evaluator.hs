module HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Evaluator (
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Instruction

import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Parser

import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelIO.Containers.LLIndexSafe

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> LoopLimit -> DumpType -> m ()
evalSource source tape _limit dt = logDump dt =<< flip runList tape =<< parseWithOptimize source

runList :: (BIO m , Symbol e) => FastInstructionList -> FullTape e -> m $ Automaton e
runList il = nextStep (IU il 0)

nextStep :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ Automaton e
nextStep (IU iv ic) = doInstructionOpt (iv `indexMaybe` ic) (IU iv $ ic + 1)

doInstructionOpt :: (BIO m , Symbol e) => Maybe FastInstruction -> InstructionUnit -> FullTape e -> m $ Automaton e
doInstructionOpt (Just i) table tape = doInstruction i (Automaton table tape)
doInstructionOpt Nothing  table tape = doEnd           (Automaton table tape)

doInstruction :: (BIO m , Symbol e) => FastInstruction -> Automaton e -> m $ Automaton e
doInstruction (Move   i             ) (Automaton table tape) = nextStep table (moveHead          i        tape)
doInstruction (Inc    i             ) (Automaton table tape) = nextStep table (incSymbol         i        tape)
doInstruction  Output                  a                     = doOutputChar a
doInstruction  Input                   a                     = doInputChar a
doInstruction (While  iv            ) (Automaton table tape) = doWhile iv table                             tape
doInstruction (Set    i             ) (Automaton table tape) = nextStep   table (setSymbol         i        tape)

doInstruction (SubClr          f    ) (Automaton table tape) = nextStep   table (subAndClearSymbol          f     tape)
doInstruction (AddClr          f    ) (Automaton table tape) = nextStep table (addAndClearSymbol          f     tape)
doInstruction (MulAddClr m     f    ) (Automaton table tape) = nextStep table (mulAddAndClearSymbol m     f     tape)

doInstruction (DupClr          f1 f2) (Automaton table tape) = nextStep table (dupAndClearSymbol          f1 f2 tape)
doInstruction (MulDupClr m1 m2 f1 f2) (Automaton table tape) = nextStep table (mulDupAndClearSymbol m1 m2 f1 f2 tape)

doInstruction (TriClr i1 i2 i3      ) (Automaton table tape) = nextStep table (triAndClearSymbol i1 i2 i3 tape)

doWhile :: (BIO m , Symbol e) => FastInstructionList -> InstructionUnit -> FullTape e -> m $ Automaton e
doWhile _  table tape@(_ , 0:_) = nextStep table tape
doWhile iv table tape           = doWhileWithTape iv table =<< runList iv tape

doWhileWithTape :: (BIO m , Symbol e) => FastInstructionList -> InstructionUnit -> Automaton e -> m $ Automaton e
doWhileWithTape iv table = doWhile iv table . unitTape

-- | IO instructions
doInputChar  :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar (Automaton table tape) = (nextStep table . flip writeSymbol tape) =<< wGetChar

doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar (Automaton _          (_ ,  [])) = error "Illegal State"
doOutputChar (Automaton table tape@(_ , e:_)) = wPutChar (toChar e) *> nextStep table tape

-- | Terminate instruction
doEnd :: BIO m => Automaton e -> m $ Automaton e
doEnd = pure

-- | Types
--type AutomatonSame e = Same (Automaton e)

data Automaton e = Automaton
  { unitUI   :: InstructionUnit
  , unitTape :: FullTape e
  }
  deriving stock (Eq , Show)

data InstructionUnit = IU !FastInstructionList !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
