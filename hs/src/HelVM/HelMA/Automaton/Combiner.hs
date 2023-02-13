module HelVM.HelMA.Automaton.Combiner where

import           HelVM.HelMA.Automaton.IO.AutomatonIO

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Loop           as Loop

import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.Combiner.ALU   as ALU
import           HelVM.HelMA.Automaton.Combiner.CPU   as CPU
import           HelVM.HelMA.Automaton.Combiner.LSU   as LSU

import           Control.Type.Operator

import           Prelude                              hiding (swap)

-- | Core of Combiner

runInstruction :: (SRAutomatonIO Symbol s r m) => Instruction -> SF s r m
runInstruction (IAL      i) a = Loop.continue . updateStack   a <$> runALI i (memoryStack a)
runInstruction (ILS      i) a = Loop.continue . updateFromLSM a <$> runSLI i (toLSM a)
runInstruction (ICF      i) a = Loop.continue . updateFromCPM a <$> runCFI i (toCPM a)
runInstruction  End         a = end a

pop2ForStack :: (SRAutomatonIO Symbol s r m) => Memory s r -> m (Symbol , Symbol , Memory s r)
pop2ForStack a = build <$> pop2 (memoryStack a) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack a s')

push1ForStack :: Stack s Symbol => Symbol -> Memory s r -> Memory s r
push1ForStack e a = a { memoryStack = push1 e (memoryStack a) }

end :: (SRAutomatonIO Symbol s r m) => SF s r m
end = pure . Loop.break

-- | Constructors

flippedNewMemory :: (s , r) -> InstructionList -> Memory s r
flippedNewMemory = flip (uncurry . newMemory)

newMemory :: InstructionList -> s -> r -> Memory s r
newMemory il = Memory (newCM il)

-- | Updaters

incrementIC :: Memory s r -> Memory s r
incrementIC m = m { memoryCM = incrementPC $ memoryCM m}

updateStack :: Memory s r -> s -> Memory s r
updateStack m s = m {memoryStack = s}

updateFromCPM :: Memory s r -> CentralProcessingMemory s -> Memory s r
updateFromCPM m cpm = m { memoryCM = controlMemory cpm, memoryStack = alm cpm}

updateFromLSM :: Memory s r -> LoadStoreMemory s r -> Memory s r
updateFromLSM m lsu = m {memoryStack = stack lsu , memoryRAM = ram lsu}

-- | Accessors

memoryProgram :: Memory s r -> InstructionVector
memoryProgram = program . memoryCM

memoryProgramCounter :: Memory s r -> InstructionCounter
memoryProgramCounter = programCounter . memoryCM

toCPM :: Memory s r -> CentralProcessingMemory s
toCPM a = CPM { controlMemory = memoryCM a , alm = memoryStack a }

toLSM :: Memory s r -> LoadStoreMemory s r
toLSM a = LSM { stack = memoryStack a, ram = memoryRAM a }

-- | Types

type SF s r m = Memory s r -> m $ MemorySame s r

type F s r m = Memory s r -> m $ Memory s r

type MemorySame s r = Same (Memory s r)

-- | Data types
data Memory s r = Memory
  { memoryCM    :: ControlMemory
  , memoryStack :: s
  , memoryRAM   :: r
  }
  deriving stock (Show)
