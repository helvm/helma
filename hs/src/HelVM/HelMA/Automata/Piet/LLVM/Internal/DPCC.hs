module HelVM.HelMA.Automata.Piet.LLVM.Internal.DPCC
  ( dpccsToBackwardDPCCTable
  ) where

import qualified Data.List                                      as List
import qualified Data.Map                                       as M
import qualified Data.Set                                       as S

import           GHC.Exts
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Cyclic
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

dpccsToBackwardDPCCTable ∷ [DPCC] → Map DPCC [DPCC]
dpccsToBackwardDPCCTable [] = M.empty
dpccsToBackwardDPCCTable possibleDPCCs = M.fromList $ nearestTableToBackwardTable $ nearestDPCCTable possibleDPCCs where
  nearestTableToBackwardTable ∷ Ord b ⇒ [(a, b)] → [(b, [a])]
  nearestTableToBackwardTable = fmap (extractGroup &&& fmap fst) . groupWith snd

  extractGroup ∷ [(a, b)] → b
  extractGroup ((_, b) : _) = b
  extractGroup []           = error "unreachable"

nearestDPCCTable ∷ [DPCC] → [(DPCC, DPCC)]
nearestDPCCTable possibleDPCCs = (id &&& nearestDPCC) <$> allDPCCs where
  nearestDPCC (DPCC dp cc) =
    let
      nearestDP = nearestDPTable M.! dp
      nearestCC = toEnum $ (fromEnum cc + fromEnum nearestDP - fromEnum dp) `mod` 2
      nearestDPCCCandidate = DPCC nearestDP nearestCC
    in if S.member nearestDPCCCandidate possibleDPCCSet
       then nearestDPCCCandidate
       else DPCC nearestDP (cyclicSucc nearestCC)

  nearestDPTable ∷ Map DirectionPointer DirectionPointer
  nearestDPTable = M.fromList $ go reversedAllDPs (cycle reversedPossibleDPs) (List.last reversedPossibleDPs) where
    go [] _ _ = []
    go (currentDP : currentDPs) (nextDP : nextDPs) dp | currentDP == nextDP = (currentDP, nextDP) : go currentDPs nextDPs nextDP
                                                      | otherwise = (currentDP, dp) : go currentDPs (nextDP : nextDPs) dp
    go _ [] _ = error "unreachable"
    reversedPossibleDPs = S.toDescList possibleDPSet
    reversedAllDPs = reverse [minBound .. maxBound]

  allDPCCs = DPCC <$> [minBound .. maxBound] <*> [minBound .. maxBound]
  possibleDPCCSet = S.fromList possibleDPCCs
  possibleDPSet = S.map getDP possibleDPCCSet
