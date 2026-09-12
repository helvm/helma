{-# LANGUAGE UnicodeSyntax #-}
module HelVM.HelMA.Automata.BrainFuck.EvaluatorBenchMark where

import           HelVM.HelMA.Automata.BrainFuck.Evaluator
import           HelVM.HelMA.Automata.BrainFuck.FileExtra

import           HelVM.HelMA.Automata.BrainFuck.API.ImplType

import           HelVM.HelMA.Automaton.Eff.Mock
import           HelVM.HelMA.Automaton.Types.CellType

import           HelVM.HelIO.CartesianProduct

import qualified Data.Sequences                              as S

import           Test.Hspec                                  hiding (it)
import           Test.Hspec.BenchGolden

cellTypes8 ∷ [CellType]
cellTypes8 = S.reverse $ take 2 $ toList cellTypes

cellTypes16 ∷ [CellType]
cellTypes16 = S.reverse $ take 4 $ toList cellTypes

cellTypes32 ∷ [CellType]
cellTypes32 = S.reverse $ take 6 $ toList cellTypes

benchMarkWith ∷ BenchConfig → Spec
benchMarkWith cfg = describe "BF" $ do
  benchMark8 cfg
  benchMark16 cfg
  benchMark32 cfg

-- | 8 bits
benchMark8 ∷ BenchConfig → Spec
benchMark8 cfg = describe "BF8" $ forM_ (cellTypes8 >*< toList implTypes) (benchMarkByCellType8 cfg)

benchMarkByCellType8 ∷ BenchConfig → BenchParams → Spec
benchMarkByCellType8 cfg benchParams = benchGoldenWith cfg (show benchParams) (nfAppIO exec8 benchParams ∷ BenchAction)

exec8 ∷ BenchParams → IO [Text]
exec8 t = forM
  [ ("helloWorld"            , "")
  , ("fascistHelloWorld"     , "")
  , ("theShortestHelloWorld" , "")
  , ("99botles"              , "")
  , ("triangle"              , "")
  ] $ exec t

-- | 16 bits
benchMark16 ∷ BenchConfig → Spec
benchMark16 cfg = describe "BF16" $ forM_ (cellTypes16 >*< toList implTypes) (benchMarkByCellType16 cfg)

benchMarkByCellType16 ∷ BenchConfig → BenchParams → Spec
benchMarkByCellType16 cfg benchParams = benchGoldenWith cfg (show benchParams) (nfAppIO exec16 benchParams ∷ BenchAction)

exec16 ∷ BenchParams → IO [Text]
exec16 t = forM
  [ ("helloWorld"            , "")
  , ("fascistHelloWorld"     , "")
--  , ("theShortestHelloWorld" , "")
--  , ("99botles"              , "")
  , ("triangle"              , "")
  ] $ exec t

-- | 32 bits
benchMark32 ∷ BenchConfig → Spec
benchMark32 cfg = describe "BF32" $ forM_ (cellTypes32 >*< toList implTypes) (benchMarkByCellType32 cfg)

benchMarkByCellType32 ∷ BenchConfig → BenchParams → Spec
benchMarkByCellType32 cfg benchParams = benchGoldenWith cfg (show benchParams) (nfAppIO exec32 benchParams ∷ BenchAction)

exec32 ∷ BenchParams → IO [Text]
exec32 t = forM
  [ ("helloWorld"            , "")
  , ("fascistHelloWorld"     , "")
  ] $ exec t

exec ∷ BenchParams → (FilePath , Text) → IO Text
exec (cellType , implType) (fileName , input) = do
  let file = readBfFile fileName
  let params = (implType ,  , cellType) <$> file
  let ioExec = ioExecMockEffWithInput input . simpleEval =<< params
  calculateOutput <$> ioExec

-- | Types
type BenchParams = (CellType , ImplType)
