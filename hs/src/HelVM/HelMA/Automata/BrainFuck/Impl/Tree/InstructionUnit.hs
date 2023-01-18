module HelVM.HelMA.Automata.BrainFuck.Impl.Tree.InstructionUnit where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Instruction

import           HelVM.HelIO.Containers.LLIndexSafe

currentInstruction :: InstructionUnit -> Maybe TreeInstruction
currentInstruction (IU iv ic) = iv `indexMaybe` ic

nextIC:: InstructionUnit -> InstructionUnit
nextIC (IU iv ic) = (IU iv $ ic + 1)

data InstructionUnit = IU !TreeInstructionVector !InstructionCounter
  deriving stock (Eq , Show)

type InstructionCounter = Int
