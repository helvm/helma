module HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction where

import qualified Text.Read
import qualified Text.Show

charToSimpleInstruction :: Char -> Maybe SimpleInstruction
charToSimpleInstruction = readMaybe . one

simpleInstructions :: [SimpleInstruction]
simpleInstructions = [Output , Input , MoveR , MoveL , Inc , Dec]

data SimpleInstruction =
    Output
  | Input
  | MoveR
  | MoveL
  | Inc
  | Dec
  deriving stock (Bounded , Enum , Eq)

instance Show SimpleInstruction where
  show Output = "."
  show Input  = ","
  show MoveR  = ">"
  show MoveL  = "<"
  show Inc    = "+"
  show Dec    = "-"

instance Read SimpleInstruction where
  readsPrec _ "." = [( Output , "")]
  readsPrec _ "," = [( Input  , "")]
  readsPrec _ ">" = [( MoveR  , "")]
  readsPrec _ "<" = [( MoveL  , "")]
  readsPrec _ "+" = [( Inc    , "")]
  readsPrec _ "-" = [( Dec    , "")]
  readsPrec _  _  = []
