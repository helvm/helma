module HelVM.HelMA.Automata.BrainFuck.Common.PureInstruction where

import qualified Text.Read
import qualified Text.Show

data PureInstruction =
    MoveR
  | MoveL
  | Inc
  | Dec
  deriving stock (Bounded , Enum , Eq)

instance Show PureInstruction where
  show MoveR = ">"
  show MoveL = "<"
  show Inc   = "+"
  show Dec   = "-"

instance Read PureInstruction where
  readsPrec _ ">" = [( MoveR  , "")]
  readsPrec _ "<" = [( MoveL  , "")]
  readsPrec _ "+" = [( Inc    , "")]
  readsPrec _ "-" = [( Dec    , "")]
  readsPrec _  _  = []
