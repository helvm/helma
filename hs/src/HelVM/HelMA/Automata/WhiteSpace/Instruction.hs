module HelVM.HelMA.Automata.WhiteSpace.Instruction where

import           HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import           HelVM.HelMA.Automata.WhiteSpace.Symbol

import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.LowControlOperator
import           HelVM.HelMA.Automaton.Operator.MemoryOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator

import           Data.Vector                                       as Vector


data Instruction =
    Stack !StackOperator
  | Binary !BinaryOperator
  | IOStack !IOOperator
  | Memory !MemoryOperator

  | Low LowControlOperator
  | End
  deriving stock (Eq , Show , Read)

type InstructionList   = [Instruction]
type InstructionVector = Vector Instruction

----

parseIndex :: OperandParser StackIndex
parseIndex = parseInt

parseSymbol :: OperandParser Symbol
parseSymbol = parseInteger

parseLabel :: Bool -> OperandParser Label
parseLabel False = parseDigitString
parseLabel True  = parseAsciiString
