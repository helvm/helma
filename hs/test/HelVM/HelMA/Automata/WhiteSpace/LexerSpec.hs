module HelVM.HelMA.Automata.WhiteSpace.LexerSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.Lexer

import           HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData
import           HelVM.HelMA.Automata.WhiteSpace.FileUtil

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                        (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "lexer" $ do
    describe "from-wsa" $
      forM_ [ "true"
            , "hello"
            , "hello2"
            , "hello4"
            , "bottles"
            , "prim"
            ] $ \ fileName -> do
        let minorPath = "from-wsa" </> fileName
        describe minorPath $
          it "minified" $
            show . readVisibleTokens <$> readStnFile minorPath `goldenShouldIO` buildAbsoluteStnFileName ("lexer" </> minorPath)

    describe "original" $
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
          it "minified" $
            show . readVisibleTokens <$> readStnFile minorPath `goldenShouldIO` buildAbsoluteStnFileName ("lexer" </> minorPath)
          it "tokenize" $
            tokenizeVisible          <$> readStnFile minorPath `shouldReturn` tl
