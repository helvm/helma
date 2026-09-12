module HelVM.HelMA.Automata.ETA.EvaluatorSpec where

import           HelVM.HelMA.Automata.ETA.Evaluator
import           HelVM.HelMA.Automata.ETA.FileExtra
import           HelVM.HelMA.Automata.ETA.SimpleParams
import           HelVM.HelMA.Automaton.API.AutomatonType

import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.CartesianProduct

import           System.FilePath.Posix

import           Test.Hspec                             hiding (it)
import           Test.Hspec.BenchGolden

spec ∷ Spec
spec = benchMarkWith helConfig

benchMarkWith ∷ BenchConfig → Spec
benchMarkWith cfg = describe "ETA" $ forM_ ([defaultAutomatonType] >*< toList stackTypes) (benchMarkByStackType cfg)

benchMarkByStackType ∷ BenchConfig → BenchParams → Spec
benchMarkByStackType cfg t = benchGoldenWith cfg (show t) (nfAppIO execAll t ∷ BenchAction)

execAll ∷ BenchParams → IO [[Text]]
execAll t = do
  fromEas  <- execFromEas t
  original <- execOriginal t
  pure $ fromEas <> original

execFromEas ∷ BenchParams → IO [[Text]]
execFromEas t = forM
  [ ("true"    , [""])
  , ("hello"   , [""])
  , ("hello2"  , [""])
  , ("hello3"  , [""])
  , ("hello4"  , [""])
  , ("readnum" , ["0\n" , "1\n"])
  , ("fact"    , ["1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n" , "9\n" ])
  , ("bottles" , [""])
  ] $ uncurry (ioExec t "from-eas")

execOriginal ∷ BenchParams → IO [[Text]]
execOriginal t = forM
  [ ("hello"   , [""])
  , ("hello2"  , [""])
  , ("fact"    , ["1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n" , "9\n"])
  , ("bottles" , [""])
  , ("crlf"    , [""])
  ] $ uncurry (ioExec t "original")

ioExec ∷ BenchParams → FilePath → FilePath → [Text] → IO [Text]
ioExec (implType , stackType) dirName fileName inputs = do
  let file = readEtaFile (dirName </> fileName)
  forM inputs $ \ input -> do
    let params = simpleParams implType stackType <$> file
    let exec = ioExecMockEffWithInput input . simpleEval =<< params
    calculateOutput <$> exec

type BenchParams = (AutomatonType, StackType)

helConfig ∷ BenchConfig
helConfig = defaultBenchConfig
  { iterations = 1
  , warmupIterations = 1
  }
