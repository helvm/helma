module HelVM.HelMA.Automata.BrainFuck.Instruction where

import           Data.DList
import qualified Data.ListLike as LL
import           Data.Vector

import qualified Text.Show

data Instruction =
    MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  | While !InstructionVector
  deriving stock (Eq , Ord)

type InstructionList   = [Instruction]
type InstructionDList  = DList Instruction
type InstructionVector = Vector Instruction

instance Show Instruction where
  show MoveR      = ">"
  show MoveL      = "<"
  show Inc        = "+"
  show Dec        = "-"
  show Output     = "."
  show Input      = ","
  show (While tl) = "[" <> (show =<< LL.toList tl) <> "]"
