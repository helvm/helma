module HelVM.HelMA.Automata.WhiteSpace.ParserSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.FileUtil
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser

import           HelVM.Common.Control.Safe
import           HelVM.Common.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                               (Spec, describe, it)

spec :: Spec
spec =
  describe "parser" $ forM_ ((
    [ "count"
    , "helloWorld"
    , "hWorld"
    , "calc"
    , "fact"
    , "hanoi"
    , "locTest"
    , "name"
    , "truthMachine"
    ] |><| ["original"]
    ) <> (
    [ "true"
    , "hello"
    , "hello2"
    , "hello4"
    , "bottles"
    , "prim"
    ] |><| ["from-wsa"]
    )) $ \ (fileName , dirName) -> do
      let path = dirName </> fileName
      describe path $ do
        it ("minified" </> path) $
          show . readVisibleTokens <$> readStnFile path `goldenShouldIO` buildAbsoluteStnFileName path
        it ("parse" </> path) $
          safeIOToPTextIO (flipParseVisible True <$> readStnFile path) `goldenShouldIO` buildAbsoluteWsIlFileName path
