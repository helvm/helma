module HelVM.HelMA.Automata.BrainFuck.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.BrainFuck.Evaluator
import HelVM.HelMA.Automata.BrainFuck.FileUtil

import HelVM.CartesianProduct
import HelVM.WrappedGoldenIO

import HelVM.HelMA.Automaton.IO.MockIO
import HelVM.HelMA.Automaton.Types.CellType

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $ do
    forM_ ([ ("value256"              , ""     )
           , ("helloWorld"            , ""     )
           , ("fascistHelloWorld"     , ""     )
           , ("padHelloWorld"         , ""     )
           , ("theShortestHelloWorld" , ""     )
           , ("99botles"              , ""     )
           , ("triangle"              , ""     )
           , ("fibonacci"             , "0\r\n")
           ] >><| [Int8Type , Word8Type] <>
           [ ("helloWorld"            , ""     )
           , ("fascistHelloWorld"     , ""     )
           , ("theShortestHelloWorld" , ""     )
           , ("99botles"              , ""     )
           , ("triangle"              , ""     )
           ] >><| [Int16Type , Word16Type] <>
           [ ("helloWorld"            , ""     )
           , ("fascistHelloWorld"     , ""     )
           ] >><| [Int32Type , Word32Type]
          ) $ \(fileName , input , cellType) -> do
      let params = ( , cellType) <$> readBfFile fileName
      let minorPath = show cellType </> fileName
      describe minorPath $ do
        it ("monadic" </> minorPath) $ do
          flipExecMockIO input . uncurryEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName ("monadic" </> minorPath)
        it ("logging" </> minorPath) $ do
          flipExecMockIO input . uncurryEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName ("logging" </> minorPath)
