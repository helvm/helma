module HelVM.HelMA.Automata.BrainFuck.Token where

import Text.Read

import qualified Text.Show

data Token =
    MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  | JmpPast
  | JmpBack
  deriving stock (Eq , Ord , Enum)

type TokenList = [Token]

instance Show Token where
  show MoveR   = ">"
  show MoveL   = "<"
  show Inc     = "+"
  show Dec     = "-"
  show Output  = "."
  show Input   = ","
  show JmpPast = "["
  show JmpBack = "]"

instance Read Token where
  readsPrec _ ">" = [( MoveR  , "")]
  readsPrec _ "<" = [( MoveL  , "")]
  readsPrec _ "+" = [( Inc    , "")]
  readsPrec _ "-" = [( Dec    , "")]
  readsPrec _ "." = [( Output , "")]
  readsPrec _ "," = [( Input  , "")]
  readsPrec _ "[" = [( JmpPast , "")]
  readsPrec _ "]" = [( JmpBack , "")]
  readsPrec _ _   = []
