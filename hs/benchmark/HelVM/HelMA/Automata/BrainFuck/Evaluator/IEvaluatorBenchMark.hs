module HelVM.HelMA.Automata.BrainFuck.Evaluator.IEvaluatorBenchMark where

import           HelVM.HelMA.Automata.BrainFuck.Evaluator.IEvaluator
import           HelVM.HelMA.Automata.BrainFuck.FileUtil

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.CellType

import qualified Data.ListLike                                       as LL

import           Gauge.Main

cellTypes8 :: [CellType]
cellTypes8 = LL.reverse $ take 2 cellTypes

cellTypes16 :: [CellType]
cellTypes16 = LL.reverse $ take 4 cellTypes

cellTypes32 :: [CellType]
cellTypes32 = LL.reverse $ take 6 cellTypes

benchMark :: Benchmark
benchMark = bgroup "IBF"
  [ benchMark8
  , benchMark16
  , benchMark32
  ]

-- | 8 bits
benchMark8 :: Benchmark
benchMark8 = bgroup "BF8" (benchMarkByCellType8 <$> cellTypes8)

benchMarkByCellType8 :: CellType -> Benchmark
benchMarkByCellType8 cellType = bench (show cellType) $ nfIO $ exec8 cellType

exec8 :: CellType -> IO [Text]
exec8 cellType = forM
  [ ("helloWorld"            , ""     )
  , ("fascistHelloWorld"     , ""     )
  , ("theShortestHelloWorld" , ""     )
  , ("99botles"              , ""     )
  , ("triangle"              , ""     )
  ]  $ \(fileName , input) -> do
  let file = readBfFile fileName
  let params = ( , cellType) <$> file
  let exec = ioExecMockIOWithInput input . uncurryEval =<< params
  calculateOutput <$> exec

-- | 16 bits
benchMark16 :: Benchmark
benchMark16 = bgroup "BF16" (benchMarkByCellType16 <$> cellTypes16)

benchMarkByCellType16 :: CellType -> Benchmark
benchMarkByCellType16 cellType = bench (show cellType) $ nfIO $ exec16 cellType

exec16 :: CellType -> IO [Text]
exec16 cellType = forM
  [ ("helloWorld"            , ""     )
  , ("fascistHelloWorld"     , ""     )
--  , ("theShortestHelloWorld" , ""     )
--  , ("99botles"              , ""     )
  , ("triangle"              , ""     )
  ]  $ \(fileName , input) -> do
  let file = readBfFile fileName
  let params = ( , cellType) <$> file
  let exec = ioExecMockIOWithInput input . uncurryEval =<< params
  calculateOutput <$> exec

-- | 32 bits
benchMark32 :: Benchmark
benchMark32 = bgroup "BF32" (benchMarkByCellType32 <$> cellTypes32)

benchMarkByCellType32 :: CellType -> Benchmark
benchMarkByCellType32 cellType = bench (show cellType) $ nfIO $ exec32 cellType

exec32 :: CellType -> IO [Text]
exec32 cellType = forM
  [ ("helloWorld"            , ""     )
  , ("fascistHelloWorld"     , ""     )
  ]  $ \(fileName , input) -> do
  let file = readBfFile fileName
  let params = ( , cellType) <$> file
  let exec = ioExecMockIOWithInput input . uncurryEval =<< params
  calculateOutput <$> exec
