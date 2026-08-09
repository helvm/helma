module HelVM.HelMA.Automata.SubLeq.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.SubLeq.Evaluator
import           HelVM.HelMA.Automata.SubLeq.FileExtra

import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Types.RAMType

import           Gauge.Main

benchMark :: Benchmark
benchMark = bgroup "SQ" (benchMarkByRamType <$> toList ramTypes)

benchMarkByRamType :: RAMType -> Benchmark
benchMarkByRamType t = bench (show t) $ nfIO $ exec t

exec :: RAMType -> IO [Text]
exec t = forM
  [ ("hello"     , "")
  , ("longHello" , "")
  ] $ \(fileName , input) -> do
    let file = readSqFile fileName
    calculateOutput <$> ((ioExecMockEffWithInput input . simpleEval t) =<< file)
