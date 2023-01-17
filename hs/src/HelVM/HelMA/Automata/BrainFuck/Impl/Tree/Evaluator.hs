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
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelIO.Containers.LLIndexSafe

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> LoopLimit -> DumpType -> m ()
evalSource source tape _limit dt = logDump dt =<< flip runVector tape =<< parseAsVector source

runVector :: (BIO m , Symbol e) => TreeInstructionVector -> FullTape e -> m $ Automaton e
runVector iv = nextStep (IU iv 0)

nextStep :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ Automaton e
nextStep (IU iv ic) = doInstructionOpt (iv `indexMaybe` ic) (IU iv $ ic + 1)

doInstructionOpt :: (BIO m , Symbol e) => Maybe TreeInstruction -> InstructionUnit -> FullTape e -> m $ Automaton e
doInstructionOpt (Just (Simple MoveR     )) table tape = nextStep     table (moveHeadRight tape)
doInstructionOpt (Just (Simple MoveL     )) table tape = nextStep     table  (moveHeadLeft tape)
doInstructionOpt (Just (Simple Inc       )) table tape = nextStep     table    (nextSymbol tape)
doInstructionOpt (Just (Simple Dec       )) table tape = nextStep     table    (prevSymbol tape)
doInstructionOpt (Just (Simple Output    )) table tape = doOutputChar (Automaton table tape)
doInstructionOpt (Just (Simple Input     )) table tape = doInputChar  (Automaton table tape)
doInstructionOpt (Just (While  iv        )) table tape = doWhile iv   (Automaton table tape)
doInstructionOpt  Nothing                   table tape = doEnd        (Automaton table tape)

--doInstruction :: (BIO m , Symbol e) => TreeInstruction -> InstructionUnit -> FullTape e -> m $ Automaton e
--doInstructionOpt (Simple MoveR ) table tape = nextStep     table (moveHeadRight tape)
--doInstructionOpt (Simple MoveL ) table tape = nextStep     table  (moveHeadLeft tape)
--doInstructionOpt (Simple Inc   ) table tape = nextStep     table    (nextSymbol tape)
--doInstructionOpt (Simple Dec   ) table tape = nextStep     table    (prevSymbol tape)
--doInstructionOpt (Simple Output) table tape = doOutputChar (Automaton table tape)
--doInstructionOpt (Simple Input ) table tape = doInputChar  (Automaton table tape)
--doInstructionOpt (While  iv    ) table tape = doWhile iv   (Automaton table tape)

-- | Control Instruction
doWhile :: (BIO m , Symbol e) => TreeInstructionVector -> Automaton e -> m $ Automaton e
doWhile _  (Automaton table tape@(_ , 0:_)) = nextStep table tape
doWhile iv (Automaton table tape          ) = doWhileWithTape =<< runVector iv tape where
  doWhileWithTape :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
  doWhileWithTape (Automaton _ tape') = doWhile iv (Automaton table tape')

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar (Automaton _          (_ ,  [])) = error "Illegal State"
doOutputChar (Automaton table tape@(_ , e:_)) = wPutChar (toChar e) *> nextStep table tape

doInputChar  :: (BIO m , Symbol e) => Automaton e-> m $ Automaton e
doInputChar (Automaton table tape) = (nextStep table . flip writeSymbol tape) =<< wGetChar

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

data InstructionUnit = IU !TreeInstructionVector !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
