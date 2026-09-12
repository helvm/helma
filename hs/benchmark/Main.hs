module Main where

import qualified HelVM.HelMA.Automata.BrainFuck.EvaluatorBenchMark  as BF
import qualified HelVM.HelMA.Automata.ETA.EvaluatorBenchMark        as ETA
import qualified HelVM.HelMA.Automata.SubLeq.EvaluatorBenchMark     as SQ
import qualified HelVM.HelMA.Automata.WhiteSpace.EvaluatorBenchMark as WS

import           Test.Hspec
import           Test.Hspec.BenchGolden

main ∷ IO ()
main = hspec $ do
  ETA.benchMarkWith helConfig
  WS.benchMarkWith  helConfig
  BF.benchMarkWith  helConfig
  SQ.benchMarkWith  helConfig

helConfig ∷ BenchConfig
helConfig = defaultBenchConfig
  { iterations = 1
  , warmupIterations = 1
  }
