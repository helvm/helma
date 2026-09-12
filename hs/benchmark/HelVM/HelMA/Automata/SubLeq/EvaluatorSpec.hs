module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec where

import           HelVM.HelMA.Automata.SubLeq.Evaluator
import           HelVM.HelMA.Automata.SubLeq.FileExtra

import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Types.RAMType

import           Test.Hspec                            hiding (it)
import           Test.Hspec.BenchGolden

spec ∷ Spec
spec = describe "SQ" $ forM_ (toList ramTypes) benchMarkByRamType

benchMarkByRamType ∷ RAMType → Spec
benchMarkByRamType t = benchGoldenWith helConfig (show t) (nfAppIO exec t ∷ BenchAction)

exec ∷ RAMType → IO [Text]
exec t = forM
  [ ("hello"     , "")
  , ("longHello" , "")
  ] $ \(fileName , input) -> do
    let file = readSqFile fileName
    calculateOutput <$> ((ioExecMockEffWithInput input . simpleEval t) =<< file)

helConfig ∷ BenchConfig
helConfig = defaultBenchConfig
  { iterations = 5
  , warmupIterations = 5
  , useRobustStatistics = True
  }
