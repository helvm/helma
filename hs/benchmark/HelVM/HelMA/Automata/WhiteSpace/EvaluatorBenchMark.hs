module HelVM.HelMA.Automata.WhiteSpace.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.WhiteSpace.Evaluator.LLEvaluator
import           HelVM.HelMA.Automata.WhiteSpace.FileUtil
import           HelVM.HelMA.Automata.WhiteSpace.SimpleParams

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.CartesianProduct

import           System.FilePath.Posix

import           Gauge.Main

benchMark :: Benchmark
benchMark = bgroup "WS" (benchMarkByStackType <$> (stackTypes |><| ramTypes))

benchMarkByStackType :: (StackType , RAMType) -> Benchmark
benchMarkByStackType t = bench (show t) $ nfIO $ exec t

exec :: (StackType , RAMType) -> IO [[Text]]
exec = simpleEvalWS

simpleEvalWS :: (StackType , RAMType) -> IO [[Text]]
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
    forM options $ \ ascii -> do
      let paramsIO = simpleParamsWithWhiteTokenType t ascii <$> file
      calculateOutput <$> (ioExecMockIOWithInput input . simpleEval =<< paramsIO)

options :: [Bool]
options = [False , True]
