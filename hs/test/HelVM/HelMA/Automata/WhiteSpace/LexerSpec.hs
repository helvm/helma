module HelVM.HelMA.Automata.WhiteSpace.LexerSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.Lexer

import HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData
import HelVM.HelMA.Automata.WhiteSpace.FileUtil

import HelVM.WrappedGoldenIO

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "lexer" $ do
    describe "from-wsa" $ do
      forM_ [ "true"
            , "hello"
            , "hello2"
            , "hello4"
            , "bottles"
            , "prim"
            ] $ \ fileName -> do
        let minorPath = "from-wsa" </> fileName
        describe minorPath $ do
          it "minified" $ do
            show . readVisibleTokens <$> readStnFile minorPath `goldenShouldReturn` buildAbsoluteStnFileName ("lexer" </> minorPath)

    describe "original" $ do
      forM_ [ ("count"        , countTL        )
            , ("helloWorld"   , helloWorldTL   )
            , ("hWorld"       , hWorldTL       )
            , ("calc"         , calcTL         )
            , ("fact"         , factTL         )
            , ("hanoi"        , hanoiTL        )
            , ("locTest"      , locTestTL      )
            , ("name"         , nameTL         )
            , ("truthMachine" , truthMachineTL )
            ] $ \(fileName , tl) -> do
        let minorPath = "original" </> fileName
        describe minorPath $ do
          it "minified" $ do
            show . readVisibleTokens <$> readStnFile minorPath `goldenShouldReturn` buildAbsoluteStnFileName ("lexer" </> minorPath)
          it "tokenize" $ do
            tokenizeVisible          <$> readStnFile minorPath `shouldReturn` tl
