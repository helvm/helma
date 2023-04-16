module HelVM.HelMA.Automaton.Optimizer.PeepholeOptimizer (
  peepholeOptimize,
) where

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Extras.Constructors
import           HelVM.HelMA.Automaton.Instruction.Extras.Patterns

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

peepholeOptimize :: InstructionList -> InstructionList
peepholeOptimize = peepholeOptimize2 . peepholeOptimize1

peepholeOptimize1 :: InstructionList -> InstructionList
peepholeOptimize1 = fix optimize where
  optimize :: (InstructionList -> InstructionList) -> InstructionList -> InstructionList
  optimize f (ConsP i : BinaryP op           : il) = optimizeImmediateBinary i op <> f il
  optimize f (ConsP i : HalibutP             : il) = optimizeHalibut i             : f il
  optimize f (ConsP i : PickP                : il) = optimizePick i                : f il
  optimize f (ConsP c : ConsP a : BranchTP t : il) = optimizeBranch t c a         <> f il
  optimize f (ConsP a : BranchTP t           : il) = optimizeBranchLabel t a      <> f il
  optimize f (ConsP a : ConsP v : StoreP     : il) = optimizeStoreID v a           : f il
  optimize f (ConsP a : LoadP                : il) = optimizeLoadD a               : f il
  optimize f (i                              : il) = i                             : f il
  optimize _                                   []  = []

peepholeOptimize2 :: InstructionList -> InstructionList
peepholeOptimize2 = fix optimize where
  optimize :: (InstructionList -> InstructionList) -> InstructionList -> InstructionList
  optimize f (ConsP c : MoveIP i : BranchTP t  : il) = optimizeBranchCondition i t c <> f il
  optimize f (MoveIP 1 : BranchTP t            : il) = branchSwapI t                  : f il
  optimize f (ConsP 0 : CopyIP i : SubP : SubP : il) = copyAdd i                     <> f il
  optimize f (ConsP 0 : MoveIP i : SubP : SubP : il) = moveAdd i                     <> f il
  optimize f (BNeIP i : SubP                   : il) = [bNeII i , discardI]          <> f il
  optimize f (ConsP d : LoadDP s : StoreP      : il) = optimizeMoveD s d              : f il
  optimize f (i                                : il) = i                              : f il
  optimize _                                     []  = []

optimizeImmediateBinary :: Integer -> BinaryOperation -> InstructionList
optimizeImmediateBinary 0 Sub = []
optimizeImmediateBinary 0 Add = []
optimizeImmediateBinary 1 Mul = []
optimizeImmediateBinary i op  = [immediateBinaryI i op]

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

copyAdd :: Index -> [Instruction]
copyAdd 0 = []
copyAdd i = [copyII (i - 1) , addI]

moveAdd :: Index -> [Instruction]
moveAdd 0 = []
moveAdd 1 = [addI]
moveAdd i = [moveII (i - 1) , addI]

optimizeStoreID :: Integer -> Integer -> Instruction
optimizeStoreID v = storeIDI v . fromIntegral

optimizeLoadD :: Integer -> Instruction
optimizeLoadD = loadDI . fromIntegral

optimizeMoveD :: Index -> Integer -> Instruction
optimizeMoveD s d = moveDI s (fromIntegral d)
