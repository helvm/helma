module HelVM.HelMA.Automata.ETA.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.ETA.Evaluator.LLEvaluator
import           HelVM.HelMA.Automata.ETA.FileUtil

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.StackType

import           System.FilePath.Posix

import           Gauge.Main

benchMark :: Benchmark
benchMark = bgroup "ETA" (benchMarkByStackType <$> stackTypes)

benchMarkByStackType :: StackType -> Benchmark
benchMarkByStackType stackType = bench (show stackType) $ nfIO $ exec stackType

exec :: StackType -> IO [[Text]]
exec stackType = do
  fromEas <- execFromEas stackType
  original <- execOriginal stackType
  pure $ fromEas <> original

execFromEas :: StackType -> IO [[Text]]
execFromEas stackType = forM
  [ ("true"    , [""])
  , ("hello"   , [""])
  , ("hello2"  , [""])
  , ("hello3"  , [""])
  , ("hello4"  , [""])
  , ("readnum" , ["0\n" , "1\n"])
  , ("fact"    , ["0\n" , "1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n" , "9\n" ])
  , ("bottles" , [""])
  ] $ \(fileName , inputs) -> do
    ioExec stackType "from-eas" fileName inputs

execOriginal :: StackType -> IO [[Text]]
execOriginal stackType = forM
  [ ("hello"   , [""])
  , ("hello2"  , [""])
  , ("fact"    , ["1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n"])
  , ("bottles" , [""])
  , ("crlf"    , [""])
  ] $ \(fileName , inputs) -> do
    ioExec stackType "original" fileName inputs

ioExec :: StackType -> FilePath -> FilePath -> [Text] -> IO [Text]
ioExec stackType dirName fileName inputs = do
  let file = readEtaFile (dirName </> fileName)
  forM inputs $ \ input -> do
    let params = (, stackType) <$> file
    calculateOutput <$> (ioExecMockIOWithInput input . uncurryEval =<< params)
