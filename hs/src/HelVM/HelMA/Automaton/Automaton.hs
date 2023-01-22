module HelVM.HelMA.Automaton.Automaton (
  runAndDumpLogs,
  run,
  flippedNewAutomaton,
  newAutomaton,
  Automaton,
) where

import           HelVM.HelMA.Automaton.API.AutoParams

import           HelVM.HelMA.Automaton.Loop                      as Loop
import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.IO.AutomatonIO

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction

import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelMA.Automaton.Units.ALU                 as Stack
import           HelVM.HelMA.Automaton.Units.CPU                 as CPU
import           HelVM.HelMA.Automaton.Units.LSU                 as LSU

import           HelVM.HelIO.Containers.LLIndexSafe

import           Control.Applicative.Tools
import           Control.Monad.Extra
import           Control.Type.Operator

import           Data.Vector                                     as Vector

import           Prelude                                         hiding (swap)

runAndDumpLogs :: (SRAutomatonIO Symbol s r m) => AutoParams -> Automaton s r ->  m ()
runAndDumpLogs p = logDump (dumpType p) <=< run (compile p) (limit p)

run :: (SRAutomatonIO Symbol s r m) => Bool -> LimitMaybe -> F s r m
run False = runI
run True  = runA

----

runA :: (SRAutomatonIO Symbol s r m) => LimitMaybe -> F s r m
runA l a = loopMWithLimit (nextStateA $ compileA a) l  a

compileA :: (SRAutomatonIO Symbol s r m) => Automaton s r -> Vector (SF s r m)
compileA = runInstruction <.> unitProgram

nextStateA :: (SRAutomatonIO Symbol s r m) => Vector (SF s r m) -> SF s r m
nextStateA fv a = flip id (incrementIC a) =<< indexSafe fv (unitProgramCounter a)

----

runI :: (SRAutomatonIO Symbol s r m) => LimitMaybe -> F s r m
runI = loopMWithLimit nextStateI

nextStateI :: (SRAutomatonIO Symbol s r m) => SF s r m
nextStateI a = nextStateForInstruction =<< currentInstruction (unitCU a) where
  nextStateForInstruction i = runInstruction i $ incrementIC a

----

runInstruction :: (SRAutomatonIO Symbol s r m) => Instruction -> SF s r m
runInstruction (IAL      i) a = Loop.continue . updateStack   a <$> runALI i (unitStack a)
runInstruction (ILS      i) a = Loop.continue . updateFromLSU a <$> runSLI i (toLSU a)
runInstruction (ICF      i) a = Loop.continue . updateFromCPU a <$> runCFI i (toCPU a)
runInstruction  Transfer    a = transfer a
runInstruction  End         a = end a

transfer :: (SRAutomatonIO Symbol s r m) => SF s r m
transfer = transferBranch <=< pop2ForStack

transferBranch :: (SRAutomatonIO Symbol s r m) => (Symbol, Symbol, Automaton s r) -> m $ AutomatonSame s r
transferBranch (_ , 0 , u) = pure $ Loop.continue u
transferBranch (0 , _ , u) = end u
transferBranch (a , _ , u) = Loop.continue . updateFromCPU u <$> runCFI dJumpI (toCPU $ push1ForStack a u)

pop2ForStack :: (SRAutomatonIO Symbol s r m) => Automaton s r -> m (Symbol , Symbol , Automaton s r)
pop2ForStack a = build <$> pop2 (unitStack a) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack a s')

push1ForStack :: Stack s Symbol => Symbol -> Automaton s r -> Automaton s r
push1ForStack e a = a { unitStack = push1 e (unitStack a) }

end :: (SRAutomatonIO Symbol s r m) => SF s r m
end = pure . Loop.break

-- | Constructors

flippedNewAutomaton :: (s , r) -> InstructionList -> Automaton s r
flippedNewAutomaton = flip (uncurry . newAutomaton)

newAutomaton :: InstructionList -> s -> r -> Automaton s r
newAutomaton il = Automaton (newCU il)

-- | Updaters

incrementIC :: Automaton s r -> Automaton s r
incrementIC a = a { unitCU = incrementPC $ unitCU a}

updateStack :: Automaton s r -> s -> Automaton s r
updateStack a s = a {unitStack = s}

updateFromCPU :: Automaton s r -> CentralProcessingUnit s -> Automaton s r
updateFromCPU a cpu = a { unitCU = controlUnit cpu, unitStack = alu cpu}

updateFromLSU :: Automaton s r -> LoadStoreUnit s r -> Automaton s r
updateFromLSU a lsu = a {unitStack = stack lsu , unitRAM = ram lsu}

-- | Accessors

unitProgram :: Automaton s r -> InstructionVector
unitProgram = program . unitCU

unitProgramCounter :: Automaton s r -> InstructionCounter
unitProgramCounter = programCounter . unitCU

toCPU :: Automaton s r -> CentralProcessingUnit s
toCPU a = CPU { controlUnit = unitCU a , alu = unitStack a }

toLSU :: Automaton s r -> LoadStoreUnit s r
toLSU a = LSU { stack = unitStack a, ram = unitRAM a }

-- | Types

type SF s r m = Automaton s r -> m $ AutomatonSame s r

type F s r m = Automaton s r -> m $ Automaton s r

type AutomatonSame s r = Same (Automaton s r)

-- | Data types
data Automaton s r = Automaton
  { unitCU    :: ControlUnit
  , unitStack :: s
  , unitRAM   :: r
  }
  deriving stock (Show)
