module HelVM.HelMA.Automaton.Optimizer.PeepholeOptimizer (
  peepholeOptimize1,
  peepholeOptimize2,
  peepholeOptimize3,
) where

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           HelVM.HelMA.Automaton.Instruction.Extras.Constructors
import           HelVM.HelMA.Automaton.Instruction.Extras.Patterns

peepholeOptimize1 :: InstructionList -> InstructionList
peepholeOptimize1 = fix optimize where
  optimize :: (InstructionList -> InstructionList) -> InstructionList -> InstructionList
  optimize f (ConsP i : BinaryP op           : il) = immediateBinaryI i op    : f il
  optimize f (ConsP i : HalibutP             : il) = optimizeHalibut i        : f il
  optimize f (ConsP i : PickP                : il) = optimizePick i           : f il
  optimize f (ConsP c : ConsP a : BranchTP t : il) = optimizeBranch t c a    <> f il
  optimize f (ConsP a : BranchTP t           : il) = optimizeBranchLabel t a <> f il
  optimize f (i                              : il) = i                        : f il
  optimize _                                   []  = []

peepholeOptimize2 :: InstructionList -> InstructionList
peepholeOptimize2 = fix optimize where
  optimize :: (InstructionList -> InstructionList) -> InstructionList -> InstructionList
  optimize f (ConsP c : MoveIP i : BranchTP t : il) = optimizeBranchCondition i t c <> f il
  optimize f (MoveIP 1 : BranchTP t           : il) = branchSwapI t                  : f il
  optimize f (i                               : il) = i                              : f il
  optimize _                                    []  = []

peepholeOptimize3 :: InstructionList -> InstructionList
peepholeOptimize3 = fix optimize where
  optimize :: (InstructionList -> InstructionList) -> InstructionList -> InstructionList
  optimize f (j@(JumpP _) : il) = j : f (dropWhile (not . isMark) il)
  optimize f (i : il)           = i : f il
  optimize _ []                 = []

optimizeHalibut :: Integer -> Instruction
optimizeHalibut i
  | 0 < i     = moveII $ fromIntegral i
  | otherwise = copyII $ fromIntegral $ negate i

optimizePick :: Integer -> Instruction
optimizePick i
  | 0 <= i    = copyII $ fromIntegral i
  | otherwise = moveII $ fromIntegral $ negate i

optimizeBranch :: BranchTest -> Integer -> Integer -> InstructionList
optimizeBranch t c a = check $ isJump t c where
  check True = [jumpII $ fromIntegral a]
  check _    = []

optimizeBranchLabel :: BranchTest -> Integer -> InstructionList
optimizeBranchLabel t a = [branchI t $ fromIntegral a]

optimizeBranchCondition :: Index -> BranchTest -> Integer -> InstructionList
optimizeBranchCondition 1 t c = optimizeBranchCondition1 t c
optimizeBranchCondition i t c = check $ isJump t c where
  check True = [moveII1 , jumpTI]
  check _    = [moveII1 , discardI]
  moveII1 = moveII (i - 1)

optimizeBranchCondition1 :: BranchTest -> Integer -> InstructionList
optimizeBranchCondition1 t c = check $ isJump t c where
  check True = [jumpTI]
  check _    = [discardI]
