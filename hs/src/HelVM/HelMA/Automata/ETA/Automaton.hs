module HelVM.HelMA.Automata.ETA.Automaton (
  run,
  newMemory,
) where

import           HelVM.HelMA.Automata.ETA.Addressing
import           HelVM.HelMA.Automata.ETA.OperandParsers
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.Loop

import           HelVM.HelMA.Automaton.IO.AutomatonIO

import           HelVM.HelMA.Automaton.Combiner.ALU      as Stack

import           Control.Monad.Extra
import           Control.Type.Operator
import           HelVM.HelMA.Automata.ETA.Symbol

import qualified Data.Vector                             as Vector

import           Prelude                                 hiding (divMod)

run :: (SAutomatonIO e s m) => Maybe Natural -> Memory s -> m $ Memory s
run = loopMWithLimit nextState

nextState :: (SAutomatonIO e s m) => Memory s -> m $ MemorySame s
nextState (Memory iu s) = build =<< nextIM iu where build (t , iu') = doInstruction t (Memory iu' s)

doInstruction :: (SAutomatonIO e s m) => Maybe Token -> Memory s -> m $ MemorySame s
-- | IO instructions
doInstruction (Just O) u                        = Left . updateStack u <$> doOutputChar2 (memoryStack u)
doInstruction (Just I) u                        = Left . updateStack u <$> doInputChar2 (memoryStack u)

-- | Stack instructions
doInstruction (Just N) (Memory iu s)            = build <$> parseNumber iu where build (symbol , iu') = Left (Memory iu' (push1 symbol s))
doInstruction (Just H) u                        = Left . updateStack u <$> halibut (memoryStack u)

-- | Arithmetic
doInstruction (Just S) u                        = Left . updateStack u <$> sub (memoryStack u)
doInstruction (Just E) u                        = Left . updateStack u <$> divMod (memoryStack u)

-- | Control
doInstruction (Just R) u                        = pure $ Left u
doInstruction (Just A) (Memory iu@(IM il ic) s) = pure $ Left ((Memory iu . flipPush1 s . genericNextLabel il) ic)
doInstruction (Just T) u                        = transfer u
doInstruction Nothing u                         = end u

transfer :: (SAutomatonIO e s m) => Memory s -> m $ MemorySame s
transfer = branch <=< pop2ForStack where
  branch (_ , 0 , u) = pure $ Left u
  branch (0 , _ , u) = end u
  branch (l , _ , u) = Left . updateAddress u <$> genericFindAddress (memoryProgram u) l

pop2ForStack :: (SAutomatonIO e s m) => Memory s -> m (e , e , Memory s)
pop2ForStack u = build <$> pop2 (memoryStack u) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack u s')

-- | Terminate instruction
end :: (SAutomatonIO e s m) => Memory s -> m $ MemorySame s
end = pure . Right

-- | Memory methods

newMemory :: TokenList -> s -> Memory s
newMemory tl = Memory (IM (Vector.fromList tl) 0)

updateStack :: Memory s -> s -> Memory s
updateStack u s =  u {memoryStack = s}

updateAddress :: Memory s -> InstructionCounter -> Memory s
updateAddress u a =  u {memoryIM = updatePC (memoryIM u) a}

memoryProgram :: Memory s -> TokenVector
memoryProgram = program . memoryIM

-- | Types

type MemorySame s = Same (Memory s)

data Memory s = Memory
  { memoryIM    :: !InstructionMemory
  , memoryStack :: s
  }
  deriving stock (Eq , Read , Show)
