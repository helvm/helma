module HelVM.HelMA.Automata.SubLeq.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.SubLeq.Evaluator.LLEvaluator
import           HelVM.HelMA.Automata.SubLeq.FileUtil

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.RAMType

import           Gauge.Main

benchMark :: Benchmark
benchMark = bgroup "SQ" (benchMarkByRamType <$> ramTypes)

benchMarkByRamType :: RAMType -> Benchmark
benchMarkByRamType t = bench (show t) $ nfIO $ exec t

exec :: RAMType -> IO [Text]
exec t = forM
  [ ("hello"     , "")
  , ("longHello" , "")
  ] $ \(fileName , input) -> do
    let file = readSqFile fileName
    calculateOutput <$> ((ioExecMockIOWithInput input . flippedEval t) =<< file)
