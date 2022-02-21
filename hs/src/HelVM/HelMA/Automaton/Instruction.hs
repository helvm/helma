module HelVM.HelMA.Automaton.Instruction where

import           HelVM.HelMA.Automaton.Instruction.ALInstruction
import           HelVM.HelMA.Automaton.Instruction.ControlInstruction
import           HelVM.HelMA.Automaton.Instruction.LSInstruction

import           Data.Vector                                          as Vector

isMark :: Label -> Instruction -> Bool
isMark l (IControl (Mark l')) = l == l'
isMark _            _         = False

-- | Types

data Instruction =
    IAL   !ALInstruction
  | ILS  !LSInstruction
  | IControl !ControlInstruction
  | End
  deriving stock (Eq , Read , Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector Instruction
