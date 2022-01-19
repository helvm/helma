module HelVM.HelMA.Automata.BrainFuck.Evaluator.IEvaluator (
  uncurryEval,
  evalParams,
  eval
) where

import           HelVM.HelMA.Automata.BrainFuck.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Parser
import           HelVM.HelMA.Automata.BrainFuck.Symbol
import           HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Types.CellType

import           HelVM.Common.Containers.LLIndexSafe

import           HelVM.Common.Control.Logger

import           Control.Type.Operator

uncurryEval :: BIO m => (Source , CellType) -> m ()
uncurryEval = uncurry eval

----

evalParams :: BIO m => EvalParams -> m ()
evalParams p = eval (source p) (cellTypeOptions p)

eval :: BIO m => Source -> CellType -> m ()
eval source Int8Type   = evalSource source (newTape :: FullTape Int8)
eval source Word8Type  = evalSource source (newTape :: FullTape Word8)
eval source Int16Type  = evalSource source (newTape :: FullTape Int16)
eval source Word16Type = evalSource source (newTape :: FullTape Word16)
eval source Int32Type  = evalSource source (newTape :: FullTape Int32)
eval source Word32Type = evalSource source (newTape :: FullTape Word32)
eval source Int64Type  = evalSource source (newTape :: FullTape Int64)
eval source Word64Type = evalSource source (newTape :: FullTape Word64)

evalSource :: (BIO m , Symbol e) => Source -> FullTape e -> m ()
evalSource source tape = evalVector' =<< parseAsVector source where
  evalVector' :: BIO m => InstructionVector -> m ()
  evalVector' iv = logMessageTuple ("iv" , show iv) *> (evalVector iv tape >>= (\ tape' -> logMessageTuple ("tape" , show tape'))) *> pass

evalVector :: (BIO m , Symbol e) => InstructionVector -> FullTape e -> m $ FullTape e
evalVector iv = nextStep (IU iv 0)

nextStep :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ FullTape e
nextStep (IU iv ic) = doInstruction (iv `indexMaybe` ic) (IU iv $ ic + 1)

doInstruction :: (BIO m , Symbol e) => Maybe Instruction -> InstructionUnit -> FullTape e -> m $ FullTape e
doInstruction (Just MoveR     ) table tape = nextStep     table (moveHeadRight tape)
doInstruction (Just MoveL     ) table tape = nextStep     table  (moveHeadLeft tape)
doInstruction (Just Inc       ) table tape = nextStep     table   (wNextSymbol tape)
doInstruction (Just Dec       ) table tape = nextStep     table   (wPrevSymbol tape)
doInstruction (Just Output    ) table tape = doOutputChar table                tape
doInstruction (Just Input     ) table tape = doInputChar  table                tape
doInstruction (Just (While iv)) table tape = doWhile iv   table                tape
doInstruction  Nothing          table tape = doEnd table tape

doWhile :: (BIO m , Symbol e) => InstructionVector -> InstructionUnit -> FullTape e -> m $ FullTape e
doWhile _  table tape@(_ , 0:_) = nextStep table tape
doWhile iv table tape           = doWhileWithTape =<< evalVector iv tape where
  doWhileWithTape :: (BIO m , Symbol e) => FullTape e -> m $ FullTape e
  doWhileWithTape = doWhile iv table

-- | IO instructions
doOutputChar :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ FullTape e
doOutputChar _          (_ ,  []) = error "Illegal State"
doOutputChar table tape@(_ , e:_) = wPutChar (toChar e) *> nextStep table tape

doInputChar  :: (BIO m , Symbol e) => InstructionUnit -> FullTape e -> m $ FullTape e
doInputChar table tape = doInputCharWithChar =<< wGetChar where
  doInputCharWithChar char = (nextStep table . writeSymbol char) tape

-- | Terminate instruction
doEnd :: BIO m => InstructionUnit -> FullTape e -> m $ FullTape e
doEnd _ = pure

-- | Types
data InstructionUnit = IU !InstructionVector !InstructionCounter
  deriving stock (Show)

type InstructionCounter = Int
