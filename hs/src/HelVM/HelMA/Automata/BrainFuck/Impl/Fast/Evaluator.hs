module HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Evaluator (
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Instruction

import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Parser

import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelIO.Containers.LLIndexSafe

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> DumpType -> m ()
evalSource source tape dt = logDump dt =<< flip runList tape =<< parseWithOptimize source

runList :: (BIO m , Symbol e) => FastInstructionList -> FullTape e -> m $ Memory e
runList il = nextStep (IM il 0)

nextStep :: (BIO m , Symbol e) => InstructionMemory -> FullTape e -> m $ Memory e
nextStep (IM iv ic) = doInstruction (iv `indexMaybe` ic) (IM iv $ ic + 1)

doInstruction :: (BIO m , Symbol e) => Maybe FastInstruction -> InstructionMemory -> FullTape e -> m $ Memory e
doInstruction (Just (Move   i       )) table tape       = nextStep     table (moveHead          i        tape)
doInstruction (Just (Inc    i       )) table tape       = nextStep     table (incSymbol         i        tape)
doInstruction (Just  Output          ) table tape       = doOutputChar table                             tape
doInstruction (Just  Input           ) table tape       = doInputChar  table                             tape
doInstruction (Just (While  iv      )) table tape       = doWhile iv   table                             tape
doInstruction (Just (Set    i       )) table tape       = nextStep     table (setSymbol         i        tape)

doInstruction (Just (SubClr          f    )) table tape = nextStep table (subAndClearSymbol          f     tape)
doInstruction (Just (AddClr          f    )) table tape = nextStep table (addAndClearSymbol          f     tape)
doInstruction (Just (MulAddClr m     f    )) table tape = nextStep table (mulAddAndClearSymbol m     f     tape)

doInstruction (Just (DupClr          f1 f2)) table tape = nextStep table (dupAndClearSymbol          f1 f2 tape)
doInstruction (Just (MulDupClr m1 m2 f1 f2)) table tape = nextStep table (mulDupAndClearSymbol m1 m2 f1 f2 tape)

doInstruction (Just (TriClr i1 i2 i3)) table tape       = nextStep     table (triAndClearSymbol i1 i2 i3 tape)
doInstruction  Nothing           table tape             = doEnd        table                             tape

doWhile :: (BIO m , Symbol e) => FastInstructionList -> InstructionMemory -> FullTape e -> m $ Memory e
doWhile _  table tape@(_ , 0:_) = nextStep table tape
doWhile iv table tape           = doWhileWithTape =<< runList iv tape where
  doWhileWithTape :: (BIO m , Symbol e) => Memory e -> m $ Memory e
  doWhileWithTape = doWhile iv table . memoryTape

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => InstructionMemory -> FullTape e -> m $ Memory e
doOutputChar _          (_ ,  []) = error "Illegal State"
doOutputChar table tape@(_ , e:_) = wPutChar (toChar e) *> nextStep table tape

doInputChar  :: (BIO m , Symbol e) => InstructionMemory -> FullTape e -> m $ Memory e
doInputChar table tape = (nextStep table . flip writeSymbol tape) =<< wGetChar

-- | Terminate instruction
doEnd :: BIO m => InstructionMemory -> FullTape e -> m $ Memory e
doEnd iu tape = pure $ Memory iu tape

-- | Types
data Memory e = Memory
  { memoryIM   :: InstructionMemory
  , memoryTape :: FullTape e
  }
  deriving stock (Eq , Show)

data InstructionMemory = IM !FastInstructionList !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
