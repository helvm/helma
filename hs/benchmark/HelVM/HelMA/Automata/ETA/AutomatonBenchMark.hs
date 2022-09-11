module HelVM.HelMA.Automata.ETA.AutomatonBenchMark where

import           HelVM.HelMA.Automata.ETA.Automaton
import           HelVM.HelMA.Automata.ETA.FileUtil

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.ZipA

import           System.FilePath.Posix

import           Gauge.Main

benchMark :: Benchmark
benchMark = bgroup "ETA" (benchMarkByStackType <$> options |><| stackTypes)

benchMarkByStackType :: BenchParams -> Benchmark
benchMarkByStackType t = bench (show t) $ nfIO $ execAll t

execAll :: BenchParams -> IO [[Text]]
execAll t = do
  fromEas  <- execFromEas t
  original <- execOriginal t
  pure $ fromEas <> original

execFromEas :: BenchParams -> IO [[Text]]
execFromEas t = forM
  [ ("true"    , [""])
  , ("hello"   , [""])
  , ("hello2"  , [""])
  , ("hello3"  , [""])
  , ("hello4"  , [""])
  , ("readnum" , ["0\n" , "1\n"])
  , ("fact"    , ["0\n" , "1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n" , "9\n" ])
  , ("bottles" , [""])
  ] $ uncurry (ioExec t "from-eas")

execOriginal :: BenchParams -> IO [[Text]]
execOriginal t = forM
  [ ("hello"   , [""])
  , ("hello2"  , [""])
  , ("fact"    , ["1\n" , "2\n" , "3\n" , "4\n" , "5\n" , "6\n" , "7\n" , "8\n"])
  , ("bottles" , [""])
  , ("crlf"    , [""])
  ] $ uncurry (ioExec t "original")

ioExec :: BenchParams -> FilePath -> FilePath -> [Text] -> IO [Text]
ioExec (compile , stackType) dirName fileName inputs = do
  let file = readEtaFile (dirName </> fileName)
  forM inputs $ \ input -> do
    let params = (compile ,  , stackType) <$> file
    let exec = ioExecMockIOWithInput input . simpleRun =<< params
    calculateOutput <$> exec

type BenchParams = (Bool, StackType)
