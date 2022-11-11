module HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Optimizer (
  optimize,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Instruction

optimize :: FastInstructionList -> FastInstructionList
optimize  (Move s1 : Move s2 : il) = optimize (Move (s1 + s2) : il)
optimize  (Inc  s1 : Inc  s2 : il) = optimize (Inc  (s1 + s2) : il)
optimize ((While [Inc (-1)]) : il) = buildClear il
optimize ((While [Inc   1 ]) : il) = buildClear il
optimize        ((While il') : il) = buildWhile (optimize il') : optimize il
optimize                  (i : il) = i : optimize il
optimize                       []  = []

buildClear :: FastInstructionList -> FastInstructionList
buildClear = optimizeSet . optimize

optimizeSet :: FastInstructionList -> FastInstructionList
optimizeSet (Inc s : il) = Set s : il
optimizeSet          il  = Set 0 : il

buildWhile :: FastInstructionList -> FastInstruction
buildWhile [Move forward , Inc change , Move back , Inc (-1)]                           = buildAdd forward back change
buildWhile [Inc (-1) , Move forward , Inc change , Move back]                           = buildAdd forward back change
buildWhile [Move f1 , Inc 1 , Move f2 , Inc 1 , Move back , Inc (-1)]                   = buildDup f1 f2 back
buildWhile [Inc (-1) , Move f1 , Inc 1 , Move f2 , Inc 1 , Move back]                   = buildDup f1 f2 back
buildWhile [Move f1 , Inc 1 , Move f2 , Inc 1 , Move f3 , Inc 1 , Move back , Inc (-1)] = buildTri f1 f2 f3 back
buildWhile [Inc (-1) , Move f1 , Inc 1 , Move f2 , Inc 1 , Move f3 , Inc 1 , Move back] = buildTri f1 f2 f3 back
buildWhile il                                                                           = While il

buildAdd :: Integer -> Integer -> Integer -> FastInstruction
buildAdd forward back change
  | forward == negate back && change == (-1) = SubClr forward
  | forward == negate back && change == 1    = AddClr forward
  | otherwise = While [Move forward , Inc change , Move back , Inc (-1)]

buildDup :: Integer -> Integer -> Integer -> FastInstruction
buildDup f1 f2 back
  | f1 + f2 == negate back = DupClr f1 f2
  | otherwise = While [Move f1 , Inc 1 , Move f2 , Inc 1 , Move back , Inc (-1)]

buildTri :: Integer -> Integer -> Integer -> Integer -> FastInstruction
buildTri f1 f2 f3 back
  | f1 + f2 + f3 == negate back = TriClr f1 f2 f3
  | otherwise = While [Move f1 , Inc 1 , Move f2 , Inc 1 , Move f3 , Inc 1 , Move back , Inc (-1)]
