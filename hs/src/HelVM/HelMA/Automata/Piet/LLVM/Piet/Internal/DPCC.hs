module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.DPCC
  ( dpccsToBackwardDPCCTable
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Cyclic
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax

import qualified Data.List.NonEmpty                                  as NE
import qualified Data.Map                                            as M
import qualified Data.Set                                            as S

import           GHC.Exts                                            ( groupWith )

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
  nearestDPCC (DPCC dp cc)
    | S.member nearestDPCCCandidate possibleDPCCSet = nearestDPCCCandidate
    | otherwise                                      = DPCC nearestDP (cyclicSucc nearestCC)
    where
      nearestDP = nearestDPTable M.! dp
      nearestCC
        | even (fromEnum nearestDP - fromEnum dp) = cc
        | otherwise                               = cyclicSucc cc
      nearestDPCCCandidate = DPCC nearestDP nearestCC

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

  allDPCCs = DPCC <$> universe <*> universe
  possibleDPCCSet = S.fromList possibleDPCCs
  possibleDPSet = S.map getDP possibleDPCCSet
