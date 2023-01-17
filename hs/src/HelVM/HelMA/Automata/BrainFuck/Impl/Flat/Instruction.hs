module HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Instruction where

import           HelVM.HelMA.Automata.BrainFuck.Common.PureInstruction
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
  readsPrec _ ">" = [( Simple $ Pure MoveR  , "")]
  readsPrec _ "<" = [( Simple $ Pure MoveL  , "")]
  readsPrec _ "+" = [( Simple $ Pure Inc    , "")]
  readsPrec _ "-" = [( Simple $ Pure Dec    , "")]
  readsPrec _ "." = [( Simple Output , "")]
  readsPrec _ "," = [( Simple Input  , "")]
  readsPrec _ "[" = [( JmpPast       , "")]
  readsPrec _ "]" = [( JmpBack       , "")]
  readsPrec _ _   = []
