module HelVM.HelMA.Automata.Piet.LLVM.Internal.Course
  ( coursesToBackwardCourseTable
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.Cyclic
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

import qualified Data.List.NonEmpty                               as NE
import qualified Data.Map                                         as M
import qualified Data.Set                                         as S

import           GHC.Exts                                         ( groupWith )

coursesToBackwardCourseTable ∷ [Course] → Map Course [Course]
coursesToBackwardCourseTable []              = M.empty
coursesToBackwardCourseTable possibleCourses = M.fromList
  . nearestTableToBackwardTable
  $ nearestCourseTable possibleCourses

nearestCourseTable ∷ [Course] → [(Course, Course)]
nearestCourseTable possibleCourses = (id &&& nearestCourse possibleCourseSet dpTable) <$> allCourses where
  possibleCourseSet = S.fromList possibleCourses
  possibleDPSet     = S.map directionPointer possibleCourseSet
  dpTable           = buildNearestDPTable (nonEmpty (S.toDescList possibleDPSet))

nearestCourse ∷ Set Course → Map DirectionPointer DirectionPointer → Course → Course
nearestCourse possibleCourseSet dpTable (Course dp cc)
  | S.member nearestCourseCandidate possibleCourseSet = nearestCourseCandidate
  | otherwise                                         = Course nearestDP (cyclicSucc nearestCC)
  where
    nearestDP              = dpTable M.! dp
    nearestCC              = computeCC nearestDP dp cc
    nearestCourseCandidate = Course nearestDP nearestCC

computeCC ∷ DirectionPointer → DirectionPointer → CodelChooser → CodelChooser
computeCC nearestDP dp cc
  | even (fromEnum nearestDP - fromEnum dp) = cc
  | otherwise                               = cyclicSucc cc

buildNearestDPTable ∷ Maybe (NE.NonEmpty DirectionPointer) → Map DirectionPointer DirectionPointer
buildNearestDPTable Nothing                      = M.empty
buildNearestDPTable (Just neReversedPossibleDPs) = M.fromList $ go reversedAllDPs (cycle possibleList) lastDP where
  lastDP         = last neReversedPossibleDPs
  possibleList   = NE.toList neReversedPossibleDPs
  reversedAllDPs = S.toDescList (S.fromList universe)

go ∷ [DirectionPointer] → [DirectionPointer] → DirectionPointer → [(DirectionPointer, DirectionPointer)]
go [] _ _ = []
go (currentDP : currentDPs) (nextDP : nextDPs) dp
  | currentDP == nextDP = (currentDP, nextDP) : go currentDPs nextDPs nextDP
  | otherwise           = (currentDP, dp)     : go currentDPs (nextDP : nextDPs) dp
go _ [] _ = error "unreachable"

nearestTableToBackwardTable ∷ Ord b ⇒ [(a, b)] → [(b, [a])]
nearestTableToBackwardTable = fmap (extractGroup &&& fmap fst) . groupWith snd

extractGroup ∷ [(a, b)] → b
extractGroup ((_, b) : _) = b
extractGroup []           = error "unreachable"

allCourses ∷ [Course]
allCourses = Course <$> universe <*> universe
