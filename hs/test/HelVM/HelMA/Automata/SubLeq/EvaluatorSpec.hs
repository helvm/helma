module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Evaluator
import HelVM.HelMA.Automata.SubLeq.EvaluatorSpecData
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.GoldenExpectations

import HelVM.HelMA.Automaton.IO.MockIO

import System.FilePath.Posix

import Test.Hspec (Spec , describe , it)

spec :: Spec
spec = do

  describe "simpleEvalIL" $ do
    forM_ [ ("helloSQIL" , helloSQIL)
          ] $ \(fileName , il)  -> do
      let mock = (safeExecMockIOBatch . simpleEvalIL) il
      describe fileName $ do
        it "monadic" $ do
          calculateOutput <$> mock `goldenShouldSafe` buildAbsoluteOutFileName ("simpleEvalIL" </> "monadic" </> fileName)
        it "logging" $ do
          calculateLogged <$> mock `goldenShouldSafe` buildAbsoluteOutFileName ("simpleEvalIL" </> "logging" </> fileName)

  describe "simpleEval" $ do
    forM_ [ ("hello"     , "" )
          , ("longHello" , "" )
          ] $ \(fileName , input)  -> do
      let mock = (ioExecMockIOWithInput input . simpleEval) =<< readSqFile fileName
      describe fileName $ do
        it "monadic" $ do
           (calculateOutput <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("simpleEval" </> "monadic" </> fileName)
        it "logging" $ do
           (calculateLogged <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("simpleEval" </> "logging" </> fileName)
