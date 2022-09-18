module HelVM.HelMA.Automata.False.Value where

import           HelVM.HelMA.Automaton.Operator.HighControlOperator
import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.MemoryOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator

import           HelVM.HelMA.Automaton.Dynamic.DynamicIntegral
import           HelVM.HelMA.Automaton.Dynamic.DynamicVar
import           HelVM.HelMA.Automaton.Operator.Util

import           HelVM.HelMA.Automaton.BIntegral

import           HelVM.HelIO.Control.Safe

-- Util Functions
emptyReg :: Integral i => [Value i]
emptyReg = replicate 26 $ IntLit 0

-- Converters
valueToBool :: Integral i => Value i -> Bool
valueToBool (IntLit i) = integralToBool i
valueToBool _          = False

-- | Types

data Value i =
    IntLit i
  | CharLit !Char

  | Var Int
  | PutVar !Char
  | GetVar !Char

  | Print String
  | Flush

  | Stack !StackOperator
  | IOStack !IOOperator
  | Memory !MemoryOperator

  | High !HighControlOperator
  | Lambda [Value i]
  deriving stock (Eq , Show , Read)

-- | Implement Type Class

instance BIntegral i => DynamicIntegral i (Value i) where
  toDynamicIntegral = IntLit
  fromDynamicIntegral (IntLit i) = pure i
  fromDynamicIntegral      _     = liftError "fromDynamicIntegral"

instance BIntegral i => DynamicVar (Value i) where
  toDynamicVar = Var . fromIntegral
  fromDynamicVar (Var i) = pure $ fromIntegral i
  fromDynamicVar      _  = liftError "fromDynamicVar"
