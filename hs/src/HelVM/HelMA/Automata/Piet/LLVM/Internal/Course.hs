module HelVM.HelMA.Automata.Piet.LLVM.Internal.Course
  ( coursesToBackwardCourseTable
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Cyclic
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

import qualified Data.List.NonEmpty                             as NE
import qualified Data.Map                                       as M
import qualified Data.Set                                       as S

import           GHC.Exts                                       ( groupWith )

coursesToBackwardCourseTable ∷ [Course] → Map Course [Course]
coursesToBackwardCourseTable [] = M.empty
coursesToBackwardCourseTable possibleCourses = M.fromList $ nearestTableToBackwardTable $ nearestCourseTable possibleCourses where
  nearestTableToBackwardTable ∷ Ord b ⇒ [(a, b)] → [(b, [a])]
  nearestTableToBackwardTable = fmap (extractGroup &&& fmap fst) . groupWith snd

  extractGroup ∷ [(a, b)] → b
  extractGroup ((_, b) : _) = b
  extractGroup []           = error "unreachable"

nearestCourseTable ∷ [Course] → [(Course, Course)]
nearestCourseTable possibleCourses = (id &&& nearestCourse) <$> allCourses where
  nearestCourse (Course dp cc)
    | S.member nearestCourseCandidate possibleCourseSet = nearestCourseCandidate
    | otherwise                                      = Course nearestDP (cyclicSucc nearestCC)
    where
      nearestDP = nearestDPTable M.! dp
      nearestCC
        | even (fromEnum nearestDP - fromEnum dp) = cc
        | otherwise                               = cyclicSucc cc
      nearestCourseCandidate = Course nearestDP nearestCC

  nearestDPTable ∷ Map DirectionPointer DirectionPointer
  nearestDPTable = buildNearestDPTable (nonEmpty (S.toDescList possibleDPSet))

  buildNearestDPTable ∷ Maybe (NE.NonEmpty DirectionPointer) → Map DirectionPointer DirectionPointer
  buildNearestDPTable Nothing = M.empty
  buildNearestDPTable (Just neReversedPossibleDPs) = M.fromList $ go reversedAllDPs (cycle possibleList) lastDP where
    lastDP = last neReversedPossibleDPs
    possibleList = NE.toList neReversedPossibleDPs
    reversedAllDPs = S.toDescList (S.fromList universe)

  go [] _ _ = []
  go (currentDP : currentDPs) (nextDP : nextDPs) dp
    | currentDP == nextDP = (currentDP, nextDP) : go currentDPs nextDPs nextDP
    | otherwise           = (currentDP, dp) : go currentDPs (nextDP : nextDPs) dp
  go _ [] _ = error "unreachable"

  allCourses = Course <$> universe <*> universe
  possibleCourseSet = S.fromList possibleCourses
  possibleDPSet = S.map directionPointer possibleCourseSet
