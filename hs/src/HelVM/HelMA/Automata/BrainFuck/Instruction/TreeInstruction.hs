module HelVM.HelMA.Automata.BrainFuck.Instruction.TreeInstruction where

import           HelVM.HelMA.Automata.BrainFuck.Instruction.SimpleInstruction

import           Data.DList
import qualified Data.ListLike                                                as LL
import           Data.Vector

import qualified Text.Show

data TreeInstruction =
    Simple SimpleInstruction
  | While !TreeInstructionVector
  deriving stock (Eq)

type TreeInstructionList   = [TreeInstruction]
type TreeInstructionDList  = DList TreeInstruction
type TreeInstructionVector = Vector TreeInstruction

instance Show TreeInstruction where
  show (Simple i) = show i
  show (While il) = "[" <> (show =<< LL.toList il) <> "]"
