module HelVM.HelMA.Automata.BrainFuck.AutomatonBenchMark where

import           HelVM.HelMA.Automata.BrainFuck.Automaton
import           HelVM.HelMA.Automata.BrainFuck.FileExtra

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.CellType

import           HelVM.HelIO.ZipA

import qualified Data.ListLike                            as LL

import           Gauge.Main

cellTypes8 :: [CellType]
cellTypes8 = LL.reverse $ take 2 cellTypes

cellTypes16 :: [CellType]
cellTypes16 = LL.reverse $ take 4 cellTypes

cellTypes32 :: [CellType]
cellTypes32 = LL.reverse $ take 6 cellTypes

benchMark :: Benchmark
benchMark = bgroup "BF"
  [ benchMark8
  , benchMark16
  , benchMark32
  ]

-- | 8 bits
benchMark8 :: Benchmark
benchMark8 = bgroup "BF8" (benchMarkByCellType8 <$> cellTypes8 |><| options)

benchMarkByCellType8 :: BenchParams -> Benchmark
benchMarkByCellType8 benchParams = bench (show benchParams) $ nfIO $ exec8 benchParams

exec8 :: BenchParams -> IO [Text]
exec8 t = forM
  [ ("helloWorld"            , ""     )
  , ("fascistHelloWorld"     , ""     )
  , ("theShortestHelloWorld" , ""     )
  , ("99botles"              , ""     )
  , ("triangle"              , ""     )
  ] $ exec t

-- | 16 bits
benchMark16 :: Benchmark
benchMark16 = bgroup "BF16" (benchMarkByCellType16 <$> cellTypes16 |><| options)

benchMarkByCellType16 :: BenchParams -> Benchmark
benchMarkByCellType16 benchParams = bench (show benchParams) $ nfIO $ exec16 benchParams

exec16 :: BenchParams -> IO [Text]
exec16 t= forM
  [ ("helloWorld"            , ""     )
  , ("fascistHelloWorld"     , ""     )
--  , ("theShortestHelloWorld" , ""     )
--  , ("99botles"              , ""     )
  , ("triangle"              , ""     )
  ] $ exec t

-- | 32 bits
benchMark32 :: Benchmark
benchMark32 = bgroup "BF32" (benchMarkByCellType32 <$> cellTypes32 |><| options)

benchMarkByCellType32 :: BenchParams -> Benchmark
benchMarkByCellType32 benchParams = bench (show benchParams) $ nfIO $ exec32 benchParams

exec32 :: BenchParams -> IO [Text]
exec32 t = forM
  [ ("helloWorld"            , ""     )
  , ("fascistHelloWorld"     , ""     )
  ] $ exec t

exec :: BenchParams -> (FilePath , Text) -> IO Text
exec (cellType , compile) (fileName , input) = do
  let file   = readBfFile fileName
  let params = (compile ,  , cellType) <$> file
  let ioExec = ioExecMockIOWithInput input . simpleRun =<< params
  calculateOutput <$> ioExec



-- | Types
type BenchParams = (CellType , Bool)
