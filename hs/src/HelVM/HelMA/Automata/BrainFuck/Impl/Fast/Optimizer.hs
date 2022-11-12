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
buildWhile [Move forward , Inc mul , Move back , Inc (-1)]                              = buildAdd back forward mul
buildWhile [Inc (-1) , Move forward , Inc mul , Move back]                              = buildAdd back forward mul
buildWhile [Move f1 , Inc m1 , Move f2 , Inc m2 , Move back , Inc (-1)]                 = buildDup back f1 f2 m1 m2
buildWhile [Inc (-1) , Move f1 , Inc m1 , Move f2 , Inc m2 , Move back]                 = buildDup back f1 f2 m1 m2
buildWhile [Move f1 , Inc 1 , Move f2 , Inc 1 , Move f3 , Inc 1 , Move back , Inc (-1)] = buildTri back f1 f2 f3
buildWhile [Inc (-1) , Move f1 , Inc 1 , Move f2 , Inc 1 , Move f3 , Inc 1 , Move back] = buildTri back f1 f2 f3
buildWhile il                                                                           = While il

buildAdd :: Integer -> Integer -> Integer -> FastInstruction
buildAdd back forward = build (negate back == forward) where
  build True  (-1) = SubClr        forward
  build True    1  = AddClr        forward
  build True  mul  = MulAddClr mul forward
  build False mul  = While [Move forward , Inc mul , Move back , Inc (-1)]

buildDup :: Integer -> Integer -> Integer -> Integer -> Integer -> FastInstruction
buildDup back f1 f2 = build (negate back == f1 + f2) where
  build True   1  1 = DupClr          f1 f2
  build True  m1 m2 = MulDupClr m1 m2 f1 f2
  build False m1 m2 = While [Move f1 , Inc m1 , Move f2 , Inc m2 , Move back , Inc (-1)]

buildTri :: Integer -> Integer -> Integer -> Integer -> FastInstruction
buildTri back f1 f2 f3
  | f1 + f2 + f3 == negate back = TriClr f1 f2 f3
  | otherwise = While [Move f1 , Inc 1 , Move f2 , Inc 1 , Move f3 , Inc 1 , Move back , Inc (-1)]
