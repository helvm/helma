module HelVM.HelMA.Automata.BrainFuck.Automaton.TreeAutomaton (
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Instruction.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Instruction.TreeInstruction
import           HelVM.HelMA.Automata.BrainFuck.Parser
import           HelVM.HelMA.Automata.BrainFuck.Symbol
import           HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.Common.Containers.LLIndexSafe

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> DumpType -> m ()
evalSource source tape dt = logDump dt =<< flip evalVector tape =<< parseAsVector source

evalVector :: (BIO m , Symbol e) => TreeInstructionVector -> FullTape e -> m $ Unit e
evalVector iv = nextStep (IU iv 0)

nextStep :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ Unit e
nextStep (IU iv ic) = doInstruction (iv `indexMaybe` ic) (IU iv $ ic + 1)

doInstruction :: (BIO m , Symbol e) => Maybe TreeInstruction -> InstructionUnit -> FullTape e -> m $ Unit e
doInstruction (Just (Simple MoveR     )) table tape = nextStep     table (moveHeadRight tape)
doInstruction (Just (Simple MoveL     )) table tape = nextStep     table  (moveHeadLeft tape)
doInstruction (Just (Simple Inc       )) table tape = nextStep     table   (wNextSymbol tape)
doInstruction (Just (Simple Dec       )) table tape = nextStep     table   (wPrevSymbol tape)
doInstruction (Just (Simple Output    )) table tape = doOutputChar table                tape
doInstruction (Just (Simple Input     )) table tape = doInputChar  table                tape
doInstruction (Just (While  iv        )) table tape = doWhile iv   table                tape
doInstruction  Nothing                   table tape = doEnd        table                tape

doWhile :: (BIO m , Symbol e) => TreeInstructionVector -> InstructionUnit -> FullTape e -> m $ Unit e
doWhile _  table tape@(_ , 0:_) = nextStep table tape
doWhile iv table tape           = doWhileWithTape =<< evalVector iv tape where
  doWhileWithTape :: (BIO m , Symbol e) => Unit e -> m $ Unit e
  doWhileWithTape = doWhile iv table . unitTape

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ Unit e
doOutputChar _          (_ ,  []) = error "Illegal State"
doOutputChar table tape@(_ , e:_) = wPutChar (toChar e) *> nextStep table tape

doInputChar  :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ Unit e
doInputChar table tape = (nextStep table . flip writeSymbol tape) =<< wGetChar

-- | Terminate instruction
doEnd :: BIO m => InstructionUnit -> FullTape e -> m $ Unit e
doEnd iu tape = pure $ Unit iu tape

-- | Types
data Unit e = Unit
  { unitUI   :: InstructionUnit
  , unitTape :: FullTape e
  }
  deriving stock (Eq , Show)

data InstructionUnit = IU !TreeInstructionVector !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
