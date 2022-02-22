module HelVM.HelMA.Automata.SubLeq.AutomatonBenchMark where

import           HelVM.HelMA.Automata.SubLeq.Automaton
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
    calculateOutput <$> ((ioExecMockIOWithInput input . simpleEval t) =<< file)
