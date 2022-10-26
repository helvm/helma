module HelVM.HelMA.Automata.BrainFuck.Flat.Instruction where

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction

import           Text.Read

import qualified Text.Show

data FlatInstruction =
    Simple SimpleInstruction
  | JmpPast
  | JmpBack
  deriving stock (Eq)

type FlatTreeInstructionList = [FlatInstruction]

instance Show FlatInstruction where
  show (Simple i) = show i
  show JmpPast    = "["
  show JmpBack    = "]"

instance Read FlatInstruction where
  readsPrec _ ">" = [( Simple MoveR  , "")]
  readsPrec _ "<" = [( Simple MoveL  , "")]
  readsPrec _ "+" = [( Simple Inc    , "")]
  readsPrec _ "-" = [( Simple Dec    , "")]
  readsPrec _ "." = [( Simple Output , "")]
  readsPrec _ "," = [( Simple Input  , "")]
  readsPrec _ "[" = [( JmpPast       , "")]
  readsPrec _ "]" = [( JmpBack       , "")]
  readsPrec _ _   = []
