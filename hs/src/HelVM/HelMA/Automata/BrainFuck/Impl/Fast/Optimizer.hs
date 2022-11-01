module HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Optimizer (
  optimize,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Instruction

optimize :: FastInstructionList -> FastInstructionList
optimize  (Move s1 : Move s2 : il) = optimize (Move (s1 + s2) : il)
optimize  (Inc  s1 : Inc  s2 : il) = optimize (Inc  (s1 + s2) : il)
optimize ((While [Inc (-1)]) : il) = buildClear il
optimize ((While [Inc   1 ]) : il) = buildClear il
optimize        ((While il') : il) = While (optimize il') : optimize il
optimize                  (i : il) = i : optimize il
optimize                       []  = []

buildClear :: FastInstructionList -> FastInstructionList
buildClear = optimizeSet . optimize

optimizeSet :: FastInstructionList -> FastInstructionList
optimizeSet (Inc s : il) = Set s : il
optimizeSet          il  = Clear : il
