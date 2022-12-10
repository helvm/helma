module HelVM.HelMA.Automaton.Units.Unit where

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Units.CPU   as CPU
import           HelVM.HelMA.Automaton.Units.LSU   as LSU

-- | Constructors

newUnit :: InstructionList -> s -> r -> Unit s r
newUnit il = Unit (newCU il)

-- | Updaters

incrementIC :: Unit s r -> Unit s r
incrementIC u = u { unitCU = incrementPC $ unitCU u}

updateStack :: Unit s r -> s -> Unit s r
updateStack u s = u {unitStack = s}

updateFromCPU :: Unit s r -> CentralProcessingUnit s -> Unit s r
updateFromCPU u cpu = u { unitCU = controlUnit cpu, unitStack = alu cpu}

updateFromLSU :: Unit s r -> LoadStoreUnit s r -> Unit s r
updateFromLSU u lsu = u {unitStack = stack lsu , unitRAM = ram lsu}

-- | Getters

toCPU :: Unit s r -> CentralProcessingUnit s
toCPU u = CPU { controlUnit = unitCU u , alu = unitStack u }

toLSU :: Unit s r -> LoadStoreUnit s r
toLSU u = LSU { stack = unitStack u, ram = unitRAM u }

-- | Data types
data Unit s r = Unit
  { unitCU    :: ControlUnit
  , unitStack :: s
  , unitRAM   :: r
  }
  deriving stock (Show)
