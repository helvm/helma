module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Evaluator
import HelVM.HelMA.Automata.SubLeq.EvaluatorSpecData
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.WrappedGoldenIO

import HelVM.HelMA.Automaton.IO.MockIO

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do

  describe "simpleEvalIL" $ do
    forM_ [ ("helloSQIL" , helloSQIL)
          ] $ \(fileName , il)  -> do
      describe fileName $ do
        it "monadic"  $ do (batchExecMockIO . simpleEvalIL) il `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalIL" </> "monadic" </> fileName)
        it "monadic"  $ do (batchEvalMockIO . simpleEvalIL) il `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalIL" </> "logging" </> fileName)

  describe "simpleEval" $ do
    forM_ [ ("hello"     , "" )
          , ("longHello" , "" )
          ] $ \(fileName , input)  -> do
      let params = readSqFile fileName
      describe fileName $ do
        it "monadic"  $ do flipExecMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "monadic" </> fileName)
        it "logging"  $ do flipEvalMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "logging" </> fileName)
