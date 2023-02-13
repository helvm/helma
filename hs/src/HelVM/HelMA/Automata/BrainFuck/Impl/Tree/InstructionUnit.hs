module HelVM.HelMA.Automata.BrainFuck.Impl.Tree.InstructionUnit where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Instruction

import           HelVM.HelIO.Containers.LLIndexSafe

currentInstruction :: InstructionMemory -> Maybe TreeInstruction
currentInstruction (IM iv ic) = iv `indexMaybe` ic

nextIC:: InstructionMemory -> InstructionMemory
nextIC (IM iv ic) = IM iv $ ic + 1

data InstructionMemory = IM !TreeInstructionVector !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
