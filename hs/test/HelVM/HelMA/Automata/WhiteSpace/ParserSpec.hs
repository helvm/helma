module HelVM.HelMA.Automata.WhiteSpace.ParserSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData
import HelVM.HelMA.Automata.WhiteSpace.Parser
import HelVM.HelMA.Automata.WhiteSpace.FileUtil

import HelVM.Expectations
import HelVM.WrappedGoldenIO

import HelVM.Common.Safe

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    describe "from-wsa" $ do
      let majorPath = "parse" </> "from-wsa"
      forM_ [ "true"
            , "hello"
            , "hello2"
            , "hello4"
            , "bottles"
            , "prim"
            ] $ \ fileName -> do
        let minorPath = "from-wsa" </> fileName
        it minorPath $ do 
          (show . unsafe . flipParseVisible True <$> readStnFile minorPath) `goldenShouldReturn` buildAbsoluteIlFileName (majorPath </> minorPath)

    describe "original" $ do
      let majorPath = "parse" </> "from-wsa"
      forM_ [ "count"
            , "helloWorld"
            , "hWorld"
            , "calc"
            , "fact"
            , "hanoi"
            , "locTest"
            , "name"
            , "truthMachine"
            ] $ \ fileName -> do
        let minorPath = "original" </> fileName
        it minorPath $ do 
          (show . unsafe . flipParseVisible True <$> readStnFile minorPath) `goldenShouldReturn` buildAbsoluteIlFileName (majorPath </> minorPath)

    describe "parseTL" $ do
      it "cat"          $ do parseTL catTL          False `shouldSafe` catIL
      it "helloWorld"   $ do parseTL helloWorldTL   False `shouldSafe` helloWorldIL
      it "truthMachine" $ do parseTL truthMachineTL False `shouldSafe` truthMachineIL
