module HelVM.HelMA.Automata.WhiteSpace.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.WhiteSpace.Evaluator
import           HelVM.HelMA.Automata.WhiteSpace.FileExtra
import           HelVM.HelMA.Automata.WhiteSpace.SimpleParams

import           HelVM.HelMA.Automaton.API.LabelType
import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.CartesianProduct

import           System.FilePath.Posix

import           Test.Hspec                                   hiding (it)
import           Test.Hspec.BenchGolden

benchMarkWith ∷ BenchConfig → Spec
benchMarkWith cfg = describe "WS" $ forM_ (toList stackTypes >*< toList ramTypes) (benchMarkByStackType cfg)

benchMarkByStackType ∷ BenchConfig → BenchParams → Spec
benchMarkByStackType cfg t = benchGoldenWith cfg (show t) (nfAppIO exec t ∷ BenchAction)

exec ∷ BenchParams → IO [[Text]]
exec = simpleEvalWS

simpleEvalWS ∷ BenchParams → IO [[Text]]
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
    forM (toList labelTypes) $ \ ascii -> do
      let paramsIO = simpleParamsWithWhiteTokenType t ascii <$> file
      calculateOutput <$> (ioExecMockEffWithInput input . simpleEval =<< paramsIO)

type BenchParams = (StackType , RAMType)
