module HelVM.HelMA.Automaton.Optimizer (
  optimize,
  constantFolding,
) where

import           HelVM.HelMA.Automaton.API.OptimizationLevel

import           HelVM.HelMA.Automaton.Combiner.ALU

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Instruction.SInstruction

import qualified Data.ListLike                                  as LL

optimize :: OptimizationLevel -> InstructionList -> InstructionList
optimize NoOptimizations    = id
optimize BasicOptimizations = constantFolding
optimize SomeOptimizations  = peepholeOptimize . constantFolding
optimize AllOptimizations   = peepholeOptimize . constantFolding

constantFolding :: InstructionList -> InstructionList
constantFolding = constantFoldingWithAcc []

constantFoldingWithAcc :: [Integer] -> InstructionList -> InstructionList
constantFoldingWithAcc acc (i : il) = constantFoldingForI acc il i
constantFoldingWithAcc acc []       = generateIL acc

constantFoldingForI :: [Integer] -> InstructionList -> Instruction -> InstructionList
constantFoldingForI acc il i@(IAL (SAL i')) = constantFoldingForResult il i acc $ runSAL i' acc
constantFoldingForI acc il i                = generateIL acc <> (i : constantFolding il)

constantFoldingForResult :: InstructionList -> Instruction -> [Integer] -> Either a [Integer] -> InstructionList
constantFoldingForResult il _ _   (Right acc) = constantFoldingWithAcc acc il
constantFoldingForResult il i acc (Left  _  ) = generateIL acc <> (i : constantFolding il)

generateIL :: [Integer] -> InstructionList
generateIL acc = consI <$> LL.reverse acc

peepholeOptimize :: InstructionList -> InstructionList
peepholeOptimize = id
