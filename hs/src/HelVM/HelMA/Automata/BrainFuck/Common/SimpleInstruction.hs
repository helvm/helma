module HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction where

import           HelVM.HelMA.Automata.BrainFuck.Common.PureInstruction

import qualified Text.Read
import qualified Text.Show


charToSimpleInstruction :: Char -> Maybe SimpleInstruction
charToSimpleInstruction = readMaybe . one

simpleInstructions :: [SimpleInstruction]
simpleInstructions = [Output , Input , Pure MoveR , Pure MoveL , Pure Inc , Pure Dec]

data SimpleInstruction =
    Pure PureInstruction
  | Output
  | Input
  deriving stock Eq

instance Show SimpleInstruction where
  show (Pure i) = show i
  show Output   = "."
  show Input    = ","

instance Read SimpleInstruction where
  readsPrec _ ">" = [( Pure MoveR  , "")]
  readsPrec _ "<" = [( Pure MoveL  , "")]
  readsPrec _ "+" = [( Pure Inc    , "")]
  readsPrec _ "-" = [( Pure Dec    , "")]
  readsPrec _ "." = [( Output , "")]
  readsPrec _ "," = [( Input  , "")]
  readsPrec _  _  = []
