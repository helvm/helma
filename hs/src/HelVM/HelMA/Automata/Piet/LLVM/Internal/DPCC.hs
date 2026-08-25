module HelVM.HelMA.Automata.Piet.LLVM.Internal.DPCC
  ( dpccsToBackwardDPCCTable
  ) where

import           Control.Arrow
import           Data.Map                                       ( Map )
import qualified Data.Map                                       as M
import qualified Data.Set                                       as S
import           GHC.Exts
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Cyclic
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

-- | Get a map containing a key DPCC that references DPCCs which will be switched to the key in the next step.
-- The first argument is possible DPCCs to move to next blocks.
--
-- When given @[(DPRight, CCLeft), (DPUp, CCLeft)]@,
-- only @(DPRight, CCLeft)@ and @(DPRight, CCRight)@ in the current step will be switched to @(DPRight, CCLeft)@ in the next step.
dpccsToBackwardDPCCTable ∷ [DPCC] → Map DPCC [DPCC]
dpccsToBackwardDPCCTable [] = M.empty
dpccsToBackwardDPCCTable possibleDPCCs = M.fromList $ nearestTableToBackwardTable $ nearestDPCCTable possibleDPCCs where
  nearestTableToBackwardTable ∷ Ord b ⇒ [(a, b)] → [(b, [a])]
  nearestTableToBackwardTable = fmap (snd . head &&& fmap fst) . groupWith snd

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
  nearestDPTable = M.fromList $ go reversedAllDPs (cycle reversedPossibleDPs) (last reversedPossibleDPs) where
    go [] _ _ = []
    go (currentDP : currentDPs) (nextDP : nextDPs) dp | currentDP == nextDP = (currentDP, nextDP) : go currentDPs nextDPs nextDP
                                                      | otherwise = (currentDP, dp) : go currentDPs (nextDP : nextDPs) dp
    go _ [] _ = error "unreachable"
    reversedPossibleDPs = S.toDescList possibleDPSet
    reversedAllDPs = reverse [minBound .. maxBound]

  allDPCCs = DPCC <$> [minBound .. maxBound] <*> [minBound .. maxBound]
  possibleDPCCSet = S.fromList possibleDPCCs
  possibleDPSet = S.map getDP possibleDPCCSet
