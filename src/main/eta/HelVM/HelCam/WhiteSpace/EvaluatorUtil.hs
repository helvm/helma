module HelVM.HelCam.WhiteSpace.EvaluatorUtil where

import HelVM.HelCam.WhiteSpace.Instruction
import HelVM.HelCam.Common.Util
import Data.List

type InstructionCounter = Address
newtype InstructionStack = IS [Address]
type Memory = [Value]
newtype DataStack = DS Memory
newtype Heap = H Memory

data InstructionControl = IC InstructionCounter InstructionStack InstructionList

doBinary :: BinaryOperator -> Value -> Value -> Value
doBinary Add v v' = v' + v
doBinary Sub v v' = v' - v
doBinary Mul v v' = v' * v
doBinary Div v v' = v' `div` v
doBinary Mod v v' = v' `mod` v

doBranchTest :: BranchTest -> Value -> Bool
doBranchTest EZ  value = value == 0
doBranchTest Neg value = value < 0

findAddress :: InstructionList -> Identifier -> Address
findAddress = findAddress' 0

findAddress' :: Address -> InstructionList -> Identifier -> Address
findAddress' _       []                  identifier = error $ "Undefined identifier (" ++ show identifier  ++ ")"
findAddress' address ((Label identifier'):il) identifier
  | identifier == identifier'                       = address
  | otherwise                                       = findAddress' (address+1) il identifier
findAddress' address (_:il)              identifier = findAddress' (address+1) il identifier

load :: Value -> Heap -> Value
load pointer (H h) = genericIndex h pointer

store :: Value -> Value -> Heap -> Heap
store value pointer (H heap) = H $ store' value pointer heap where
  store' v 0 []     = [v]
  store' v 0 (_:h)  = v:h
  store' v p []     = 0 : (store' v (p-1) [])
  store' v p (v':h) = v': (store' v (p-1) h)

storeNum :: String -> Value -> Heap -> Heap
storeNum line = store (readOrError line :: Integer)
