module HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Evaluator (
  evalSource,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols
--import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols          (FullTape)
--import qualified HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols          as Tape

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelIO.Containers.LLIndexSafe

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> LoopLimit -> DumpType -> m ()
evalSource source tape _limit dt = logDump dt =<< flip runVector tape =<< parseAsVector source

runVector :: (BIO m , Symbol e) => TreeInstructionVector -> FullTape e -> m $ Automaton e
runVector iv = nextStep (IU iv 0)

nextStep :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ Automaton e
nextStep  (IU iv ic) tape = doInstructionOpt (iv `indexMaybe` ic) (Automaton (IU iv $ ic + 1) tape)

doInstructionOpt :: (BIO m , Symbol e) => Maybe TreeInstruction -> Automaton e -> m $ Automaton e
doInstructionOpt (Just i) = doInstruction i
doInstructionOpt  Nothing = pure

doInstruction :: (BIO m , Symbol e) => TreeInstruction -> Automaton e -> m $ Automaton e
doInstruction (Simple MoveR ) (Automaton table tape) = nextStep      table (moveHeadRight tape)
doInstruction (Simple MoveL ) (Automaton table tape) = nextStep      table  (moveHeadLeft tape)
doInstruction (Simple Inc   ) (Automaton table tape) = nextStep      table    (nextSymbol tape)
doInstruction (Simple Dec   ) (Automaton table tape) = nextStep      table    (prevSymbol tape)
doInstruction (Simple Output) a                      = doOutputChar a
doInstruction (Simple Input ) a                      = doInputChar  a
doInstruction (While  iv    ) a                      = doWhile iv   a

-- | Control Instruction
doWhile :: (BIO m , Symbol e) => TreeInstructionVector -> Automaton e -> m $ Automaton e
doWhile _  (Automaton table tape@(_ , 0:_)) = nextStep table tape
doWhile iv (Automaton table tape          ) = doWhileWithTape =<< runVector iv tape where
  doWhileWithTape :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
  doWhileWithTape (Automaton _ tape') = doWhile iv (Automaton table tape')

-- | IO instructions
doInputChar  :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar (Automaton table tape) = (nextStep table . flip writeSymbol tape) =<< wGetChar

doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar (Automaton _          (_ ,  [])) = error "Illegal State"
doOutputChar (Automaton table tape@(_ , e:_)) = wPutChar (toChar e) *> nextStep table tape

-- | Types
--type AutomatonSame e = Same (Automaton e)

data Automaton e = Automaton
  { unitUI   :: InstructionUnit
  , unitTape :: FullTape e
  }
  deriving stock (Eq , Show)

data InstructionUnit = IU !TreeInstructionVector !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
