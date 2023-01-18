module HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Evaluator (
  evalSource,
  currentSymbolSafe,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser

import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.InstructionUnit (InstructionUnit (IU))
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.InstructionUnit as IU

import           HelVM.HelMA.Automata.BrainFuck.Common.PureInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction
import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols
import qualified HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols      as Tape

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> LoopLimit -> DumpType -> m ()
evalSource source tape _limit dt = logDump dt =<< flip runVector tape =<< parseAsVector source

runVector :: (BIO m , Symbol e) => TreeInstructionVector -> FullTape e -> m $ Automaton e
runVector iv = nextStep (IU iv 0)

nextStep :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ Automaton e
nextStep iu tape = nextStepA (Automaton iu tape)

nextStepA :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
nextStepA a = doInstructionOpt (currentInstruction a) (nextIC a)

doInstructionOpt :: (BIO m , Symbol e) => Maybe TreeInstruction -> Automaton e -> m $ Automaton e
doInstructionOpt (Just i) = doInstruction i
doInstructionOpt  Nothing = pure

doInstruction :: (BIO m , Symbol e) => TreeInstruction -> Automaton e -> m $ Automaton e
doInstruction (While  iv    ) a    = doWhile iv   a
doInstruction (Simple Output) a    = doOutputChar a
doInstruction (Simple Input ) a    = doInputChar  a
doInstruction (Simple (Pure i )) a = nextStepA $    updateTape (doPure i) a

-- | Control Instruction
doWhile :: (BIO m , Symbol e) => TreeInstructionVector -> Automaton e -> m $ Automaton e
doWhile _  (Automaton table tape@(_ , 0:_)) = nextStep table tape
doWhile iv (Automaton table tape          ) = doWhileWithTape iv table =<< runVector iv tape

doWhileWithTape :: (BIO m , Symbol e) => TreeInstructionVector -> InstructionUnit -> Automaton e -> m $ Automaton e
doWhileWithTape iv table (Automaton _ tape) = doWhile iv (Automaton table tape)

-- | IO instructions
doInputChar  :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doInputChar (Automaton table tape) = (nextStep table . flip writeSymbol tape) =<< wGetChar

doOutputChar :: (BIO m , Symbol e) => Automaton e -> m $ Automaton e
doOutputChar (Automaton _          (_ ,  [])) = error "Illegal State"
doOutputChar (Automaton table tape@(_ , e:_)) = wPutChar (toChar e) *> nextStep table tape

-- | Pure Instructions
doPure :: Symbol e => PureInstruction -> FullTapeD e
doPure MoveR = Tape.moveHeadRight
doPure MoveL = Tape.moveHeadLeft
doPure Inc   = Tape.nextSymbol
doPure Dec   = Tape.prevSymbol

-- | Accessors

currentSymbolSafe :: MonadSafe m => Automaton e -> m e
currentSymbolSafe = Tape.readSymbolSafe . unitTape

currentInstruction :: Automaton e -> Maybe TreeInstruction
currentInstruction = IU.currentInstruction . unitUI

-- | Constructors

nextIC :: Automaton e -> Automaton e
nextIC = updateUI IU.nextIC

-- |

updateUI :: (InstructionUnit -> InstructionUnit) -> Automaton e -> Automaton e
updateUI f a = a { unitUI = f $ unitUI a }

updateTape :: (FullTape e -> FullTape e) -> Automaton e -> Automaton e
updateTape f a = a { unitTape = f $ unitTape a }

-- | Types
--type AutomatonSame e = Same (Automaton e)

data Automaton e = Automaton
  { unitUI   :: InstructionUnit
  , unitTape :: FullTape e
  }
  deriving stock (Eq , Show)
