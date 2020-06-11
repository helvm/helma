module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Evaluator
import HelVM.HelMA.Automata.SubLeq.EvaluatorSpecData
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.HelMA.Automata.Expectations

import HelVM.HelMA.Common.IO.MockIO

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do

  describe "simpleEvalIL" $ do
    forM_ [ ("helloSQIL" , helloSQIL)
          ] $ \(fileName , il)  -> do
      describe fileName $ do
        it "interact" $ do  batchSimpleEvalIL               il `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalIL" </> "interact" </> fileName)
        it "monadic"  $ do (batchExecMockIO . simpleEvalIL) il `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalIL" </> "monadic"  </> fileName)

  describe "simpleEval" $ do
    forM_ [ ("hello"     , "" )
          , ("longHello" , "" )
          ] $ \(fileName , input)  -> do
      let params = readSqFile fileName
      describe fileName $ do
        it "interact" $ do flipSimpleEval input              <$> params `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "interact" </> fileName)
        it "monadic"  $ do flipExecMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "monadic"  </> fileName)
