module HelVM.HelMA.Automaton.Optimizer.DeadCodeOptimizer (
  deadCodeElimination,
) where

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Instruction.Extras.Patterns


deadCodeElimination :: InstructionList -> InstructionList
deadCodeElimination = fix optimize where
  optimize :: (InstructionList -> InstructionList) -> InstructionList -> InstructionList
  optimize f (j@(JumpP _) : il) = j : f (dropWhile (not . isMark) il)
  optimize f (i : il)           = i : f il
  optimize _ []                 = []
