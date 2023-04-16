module HelVM.HelMA.Automaton.Optimizer (
  optimize,
  constantFolding,
) where

import           HelVM.HelMA.Automaton.API.OptimizationLevel

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Optimizer.ConstantFoldingOptimizer
import           HelVM.HelMA.Automaton.Optimizer.DeadCodeOptimizer
import           HelVM.HelMA.Automaton.Optimizer.MarkRemovingOptimizer
import           HelVM.HelMA.Automaton.Optimizer.PeepholeOptimizer

optimize :: OptimizationLevel -> InstructionList -> InstructionList
optimize NoOptimizations    = id
optimize BasicOptimizations = constantFolding
optimize SomeOptimizations  = deadCodeElimination . peepholeOptimize . constantFolding
optimize AllOptimizations   = deadCodeElimination . peepholeOptimize . makrRemoving . constantFolding
