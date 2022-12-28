module HelVM.HelMA.Automata.WhiteSpace.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.WhiteSpace.Evaluator
import           HelVM.HelMA.Automata.WhiteSpace.FileExtra
import           HelVM.HelMA.Automata.WhiteSpace.SimpleParams

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.ZipA

import           System.FilePath.Posix

import           Gauge.Main

benchMark :: Benchmark
benchMark = bgroup "WS" (benchMarkByStackType <$> (stackTypes |><| ramTypes))

benchMarkByStackType :: BenchParams -> Benchmark
benchMarkByStackType t = bench (show t) $ nfIO $ exec t

exec :: BenchParams -> IO [[Text]]
exec = simpleEvalWS

simpleEvalWS :: BenchParams -> IO [[Text]]
simpleEvalWS t = forM
  [ ("count"        , ""           )
  , ("hworld"       , ""           )
  , ("calc"         , "-1\n"       )
  , ("fact"         , "10\n"       )
  , ("hanoi"        , "1\n"        )
  , ("loctest"      , "1\n2\n"     )
  , ("name"         , "WriteOnly\n")
  ] $ \(fileName , input) -> do
    let file = readWsFile ("original" </> fileName)
    forM formatTypes $ \ ascii -> do
      let paramsIO = simpleParamsWithWhiteTokenType t ascii <$> file
      calculateOutput <$> (ioExecMockIOWithInput input . simpleEval =<< paramsIO)

type BenchParams = (StackType , RAMType)
