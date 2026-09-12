module HelVM.HelMA.Automata.SubLeq.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.SubLeq.Evaluator
import           HelVM.HelMA.Automata.SubLeq.FileExtra

import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Types.RAMType

import           Test.Hspec                            hiding (it)
import           Test.Hspec.BenchGolden

benchMarkWith ∷ BenchConfig → Spec
benchMarkWith cfg = describe "SQ" $ forM_ (toList ramTypes) (benchMarkByRamType cfg)

benchMarkByRamType ∷ BenchConfig → RAMType → Spec
benchMarkByRamType cfg t = benchGoldenWith cfg (show t) (nfAppIO exec t ∷ BenchAction)

exec ∷ RAMType → IO [Text]
exec t = forM
  [ ("hello"     , "")
  , ("longHello" , "")
  ] $ \(fileName , input) -> do
    let file = readSqFile fileName
    calculateOutput <$> ((ioExecMockEffWithInput input . simpleEval t) =<< file)
