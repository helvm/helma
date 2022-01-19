module HelVM.HelMA.Automata.WhiteSpace.ParserSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData
import           HelVM.HelMA.Automata.WhiteSpace.FileUtil
import           HelVM.HelMA.Automata.WhiteSpace.Parser

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           HelVM.Common.Control.Safe

import           System.FilePath.Posix

import           Test.Hspec                                        (Spec, describe, it)

spec :: Spec
spec =
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
        it minorPath $
          safeIOToPTextIO (flipParseVisible True <$> readStnFile minorPath) `goldenShouldIO` buildAbsoluteWsIlFileName (majorPath </> minorPath)

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
        it minorPath $
          safeIOToPTextIO (flipParseVisible True <$> readStnFile minorPath) `goldenShouldIO` buildAbsoluteWsIlFileName (majorPath </> minorPath)

    describe "parseTL" $ do
      it "cat"          $ parseTL catTL          False `shouldSafe` catIL
      it "helloWorld"   $ parseTL helloWorldTL   False `shouldSafe` helloWorldIL
      it "truthMachine" $ parseTL truthMachineTL False `shouldSafe` truthMachineIL
