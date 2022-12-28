module HelVM.HelMA.Automaton.Automaton (
  run,
  flippedNewAutomaton,
  newAutomaton,
  Automaton,
) where

import           HelVM.HelMA.Automaton.Loop
import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.IO.AutomatonIO

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction

import           HelVM.HelMA.Automaton.Units.ALU                 as Stack
import           HelVM.HelMA.Automaton.Units.CPU                 as CPU
import           HelVM.HelMA.Automaton.Units.LSU                 as LSU

import           Control.Monad.Extra
import           Control.Type.Operator

import           Data.Either.Extra

import           Prelude                                         hiding (swap)

run :: (SRAutomatonIO Symbol s r m) => Maybe Natural -> Automaton s r -> m $ Automaton s r
run = loopMWithLimit nextState

----

nextState :: (SRAutomatonIO Symbol s r m) => Automaton s r -> m $ AutomatonSame s r
nextState u@(Automaton cu _ _) = nextStateForInstruction =<< currentInstruction cu where
  nextStateForInstruction i = doInstruction i $ incrementIC u

----

doInstruction :: (SRAutomatonIO Symbol s r m) => Instruction -> Automaton s r -> m $ AutomatonSame s r
doInstruction (IAL      i) u = Left . updateStack   u <$> alInstruction i (unitStack u)
doInstruction (ILS      i) u = Left . updateFromLSU u <$> slInstruction i (toLSU u)
doInstruction (ICF      i) u = Left . updateFromCPU u <$> controlInstruction i (toCPU u)
doInstruction  Transfer    u = transfer u
doInstruction  End         u = end u

transfer :: (SRAutomatonIO Symbol s r m) => Automaton s r -> m $ AutomatonSame s r
transfer = transferBranch <=< pop2ForStack

transferBranch :: (SRAutomatonIO Symbol s r m) => (Symbol, Symbol, Automaton s r) -> m $ AutomatonSame s r
transferBranch (_ , 0 , u) = pure $ Left u
transferBranch (0 , _ , u) = end u
transferBranch (a , _ , u) = Left . updateFromCPU u <$> controlInstruction dJumpI (toCPU $ push1ForStack a u)

pop2ForStack :: (SRAutomatonIO Symbol s r m) => Automaton s r -> m (Symbol , Symbol , Automaton s r)
pop2ForStack u = build <$> pop2 (unitStack u) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack u s')

push1ForStack :: Stack s Symbol => Symbol -> Automaton s r -> Automaton s r
push1ForStack e u = u { unitStack = push1 e (unitStack u) }

end :: (SRAutomatonIO Symbol s r m) => Automaton s r -> m $ AutomatonSame s r
end = pure . Right

-- | Constructors

flippedNewAutomaton :: (s , r) -> InstructionList -> Automaton s r
flippedNewAutomaton = flip (uncurry . newAutomaton)

newAutomaton :: InstructionList -> s -> r -> Automaton s r
newAutomaton il = Automaton (newCU il)

-- | Updaters

incrementIC :: Automaton s r -> Automaton s r
incrementIC u = u { unitCU = incrementPC $ unitCU u}

updateStack :: Automaton s r -> s -> Automaton s r
updateStack u s = u {unitStack = s}

updateFromCPU :: Automaton s r -> CentralProcessingUnit s -> Automaton s r
updateFromCPU u cpu = u { unitCU = controlUnit cpu, unitStack = alu cpu}

updateFromLSU :: Automaton s r -> LoadStoreUnit s r -> Automaton s r
updateFromLSU u lsu = u {unitStack = stack lsu , unitRAM = ram lsu}

-- | Getters

toCPU :: Automaton s r -> CentralProcessingUnit s
toCPU u = CPU { controlUnit = unitCU u , alu = unitStack u }

toLSU :: Automaton s r -> LoadStoreUnit s r
toLSU u = LSU { stack = unitStack u, ram = unitRAM u }

-- | Types

type AutomatonSame s r = Same (Automaton s r)

-- | Data types
data Automaton s r = Automaton
  { unitCU    :: ControlUnit
  , unitStack :: s
  , unitRAM   :: r
  }
  deriving stock (Show)
