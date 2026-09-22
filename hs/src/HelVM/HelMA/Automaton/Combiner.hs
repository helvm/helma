module HelVM.HelMA.Automaton.Combiner where

import           HelVM.HelMA.Automaton.Eff.AutomatonEff

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Trampoline       as Trampoline

import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.Combiner.ALU     as ALU
import           HelVM.HelMA.Automaton.Combiner.CPU     as CPU
import           HelVM.HelMA.Automaton.Combiner.LSU     as LSU

import           Control.Type.Operator

import           Prelude                                hiding ( swap )

-- | Core of Combiner

runInstruction ∷ (SRAutomatonEff Symbol s r m) ⇒ Instruction → SF s r m
runInstruction (ISM i) !a = Trampoline.continue . updateStack   a <$> runALI i (memoryStack a)
runInstruction (ILS i) !a = Trampoline.continue . updateFromLSM a <$> runSLI i (toLSM a)
runInstruction (ICF i) !a = Trampoline.continue . updateFromCPM a <$> runCFI i (toCPM a)
runInstruction  End    !a = end a
{-# INLINE runInstruction #-}

pop2ForStack ∷ (SRAutomatonEff Symbol s r m) ⇒ Memory s r → m (Symbol , Symbol , Memory s r)
pop2ForStack a = build <$> pop2 (memoryStack a) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack a s')

push1ForStack ∷ Stack s Symbol ⇒ Symbol → Memory s r → Memory s r
push1ForStack e a = a { memoryStack = push1 e (memoryStack a) }
{-# INLINE push1ForStack #-}

end ∷ (SRAutomatonEff Symbol s r m) ⇒ SF s r m
end = pure . Trampoline.break

-- | Constructors

flippedNewMemory ∷ (s , r) → InstructionList → Memory s r
flippedNewMemory = flip (uncurry . newMemory)

newMemory ∷ InstructionList → s → r → Memory s r
newMemory il = Memory (newCM il)

-- | Updaters

incrementIC ∷ Memory s r → Memory s r
incrementIC m = m { memoryCM = incrementPC $ memoryCM m }
{-# INLINE incrementIC #-}

updateStack ∷ Memory s r → s → Memory s r
updateStack m s = m { memoryStack = s }
{-# INLINE updateStack #-}

updateFromCPM ∷ Memory s r → CentralProcessingMemory s → Memory s r
updateFromCPM m (CPM cm s) = m { memoryCM = cm, memoryStack = s }
{-# INLINE updateFromCPM #-}

updateFromLSM ∷ Memory s r → LoadStoreMemory s r → Memory s r
updateFromLSM m (LSM s r) = m { memoryStack = s, memoryRAM = r }
{-# INLINE updateFromLSM #-}

-- | Accessors

memoryProgram ∷ Memory s r → InstructionVector
memoryProgram = program . memoryCM

memoryProgramCounter ∷ Memory s r → InstructionCounter
memoryProgramCounter = programCounter . memoryCM

toCPM ∷ Memory s r → CentralProcessingMemory s
toCPM (Memory cm s _) = CPM cm s
{-# INLINE toCPM #-}

toLSM ∷ Memory s r → LoadStoreMemory s r
toLSM (Memory _ s r) = LSM s r
{-# INLINE toLSM #-}

-- | Types

type SF s r m = Memory s r → SameT m (Memory s r)

type F s r m = Memory s r → m $ Memory s r

-- | Data types
data Memory s r
  = Memory
      { memoryCM    :: !ControlMemory
      , memoryStack :: !s
      , memoryRAM   :: !r
      }
  deriving stock (Show)
