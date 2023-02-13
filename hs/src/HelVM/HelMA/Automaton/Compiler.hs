module HelVM.HelMA.Automaton.Compiler where

-- FIXME REMove this module
splitIL :: InstructionList -> [InstructionList]
splitIL = splitWhen isICF

--Split IL to Basic Block

instructionSplitter :: Splitter Instruction
instructionSplitter  = whenElt isICF
