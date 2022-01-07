module HelVM.HelMA.Automaton.Operator.StackOperator where

import           HelVM.HelMA.Automaton.Dynamic.DynamicIntegral
import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.UnaryOperator

import qualified Relude.Unsafe                                 as Unsafe

doStack :: DynamicIntegral i a => StackOperator -> [a] -> [a]
doStack Dup     (a            : s)           = a : a : s
doStack Discard (_            : s)           = s
doStack Swap    (a : a'       : s)           = a' : a : s
doStack Rot     (a : a' : a'' : s)           = a'' : a : a' : s
doStack (Dynamic Copy)    (a            : s) = pick (fromIntegral $ fromDynamicIntegralUnsafe a) s : s
doStack _     _                              = error "doStack"

pick :: Int -> [a] -> a
pick n s = Unsafe.head $ drop n s

data StackOperator =
    Liter    Integer
  | Unary   !UnaryOperator
  | Binary  !BinaryOperator
  | Static  !ManipulationOperator !StackIndex
  | Dynamic !ManipulationOperator
  | SIO     !IOOperator
  | Halibut
  | Dup
  | Rot
  | Swap
  | Discard
  deriving stock (Eq , Show , Read)

data ManipulationOperator = Copy | Move | Slide
  deriving stock (Eq , Show , Read)

type StackIndex = Int
