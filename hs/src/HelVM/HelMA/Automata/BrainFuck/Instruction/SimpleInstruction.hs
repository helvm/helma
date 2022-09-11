module HelVM.HelMA.Automata.BrainFuck.Instruction.SimpleInstruction where

import qualified Text.Read
import qualified Text.Show

charToSimpleInstruction :: Char -> Maybe SimpleInstruction
charToSimpleInstruction = readMaybe . one

simpleInstructions :: [SimpleInstruction]
simpleInstructions = [MoveR , MoveL , Inc , Dec , Output , Input]

data SimpleInstruction =
    MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  deriving stock (Bounded , Enum , Eq)

instance Show SimpleInstruction where
  show MoveR  = ">"
  show MoveL  = "<"
  show Inc    = "+"
  show Dec    = "-"
  show Output = "."
  show Input  = ","

instance Read SimpleInstruction where
  readsPrec _ ">" = [( MoveR  , "")]
  readsPrec _ "<" = [( MoveL  , "")]
  readsPrec _ "+" = [( Inc    , "")]
  readsPrec _ "-" = [( Dec    , "")]
  readsPrec _ "." = [( Output , "")]
  readsPrec _ "," = [( Input  , "")]
  readsPrec _  _  = []
