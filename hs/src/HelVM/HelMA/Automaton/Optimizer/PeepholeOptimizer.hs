module HelVM.HelMA.Automaton.Optimizer.PeepholeOptimizer (
  peepholeOptimize1,
  peepholeOptimize2,
  peepholeOptimize3,
) where

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.SInstruction

peepholeOptimize1 :: InstructionList -> InstructionList
peepholeOptimize1 (IAL (SPure (Cons i)) : IAL (SPure Halibut) : il)                                  = optimizeHalibut i : peepholeOptimize1 il
peepholeOptimize1 (IAL (SPure (Cons i)) : IAL (SPure Pick   ) : il)                                  = optimizePick    i : peepholeOptimize1 il
peepholeOptimize1 (IAL (SPure (Cons c)) : IAL (SPure (Cons a)) : ICF (Labeled (Branch t) LTop) : il) = optimizeBranch t c a <> peepholeOptimize1 il
peepholeOptimize1 (IAL (SPure (Cons a)) : ICF (Labeled (Branch t) LTop) : il)                        = optimizeBranchLabel t a <> peepholeOptimize1 il
peepholeOptimize1 (i : il)                                                                           = i : peepholeOptimize1 il
peepholeOptimize1 []                                                                                 = []

peepholeOptimize2 :: InstructionList -> InstructionList
peepholeOptimize2 (IAL (SPure (Cons c)) : IAL (SPure (Indexed Move (ImmediateO 1))) : ICF (Labeled (Branch t) LTop) : il) = optimizeBranchCondition t c <> peepholeOptimize2 il
peepholeOptimize2 (i : il)                                                                                                = i : peepholeOptimize2 il
peepholeOptimize2 []                                                                                                      = []

peepholeOptimize3 :: InstructionList -> InstructionList
peepholeOptimize3 il = map (\ i -> i) il
--peepholeOptimize3 (i : il) = i : peepholeOptimize3 il
--peepholeOptimize3 []       = []

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
optimizeBranchLabel t a = [bII t $ fromIntegral a]

optimizeBranchCondition :: BranchTest -> Integer -> InstructionList
optimizeBranchCondition t c = check $ isJump t c where
  check True = [jumpTI]
  check _    = [discardI]
