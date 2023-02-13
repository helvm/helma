module HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Evaluator (
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelIO.Containers.LLIndexSafe

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> DumpType -> m ()
evalSource source tape dt = logDump dt =<< flip runVector tape =<< parseAsVector source

runVector :: (BIO m , Symbol e) => TreeInstructionVector -> FullTape e -> m $ Memory e
runVector iv = nextStep (IM iv 0)

nextStep :: (BIO m , Symbol e) => InstructionMemory -> FullTape e -> m $ Memory e
nextStep (IM iv ic) = doInstruction (iv `indexMaybe` ic) (IM iv $ ic + 1)

doInstruction :: (BIO m , Symbol e) => Maybe TreeInstruction -> InstructionMemory -> FullTape e -> m $ Memory e
doInstruction (Just (Simple MoveR     )) table tape = nextStep     table (moveHeadRight tape)
doInstruction (Just (Simple MoveL     )) table tape = nextStep     table  (moveHeadLeft tape)
doInstruction (Just (Simple Inc       )) table tape = nextStep     table    (nextSymbol tape)
doInstruction (Just (Simple Dec       )) table tape = nextStep     table    (prevSymbol tape)
doInstruction (Just (Simple Output    )) table tape = doOutputChar table                tape
doInstruction (Just (Simple Input     )) table tape = doInputChar  table                tape
doInstruction (Just (While  iv        )) table tape = doWhile iv   table                tape
doInstruction  Nothing                   table tape = doEnd        table                tape

doWhile :: (BIO m , Symbol e) => TreeInstructionVector -> InstructionMemory -> FullTape e -> m $ Memory e
doWhile _  table tape@(_ , 0:_) = nextStep table tape
doWhile iv table tape           = doWhileWithTape =<< runVector iv tape where
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
doEnd im tape = pure $ Memory im tape

-- | Types
data Memory e = Memory
  { memoryIM   :: InstructionMemory
  , memoryTape :: FullTape e
  }
  deriving stock (Eq , Show)

data InstructionMemory = IM !TreeInstructionVector !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
