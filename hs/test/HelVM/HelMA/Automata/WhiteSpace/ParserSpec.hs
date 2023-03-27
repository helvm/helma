module HelVM.HelMA.Automata.WhiteSpace.ParserSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.FileExtra
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser

import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Optimizer
import           HelVM.HelMA.Automaton.PrettyPrinter
import           HelVM.HelMA.Automaton.Types.FormatType

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           Control.Applicative.Tools

import           System.FilePath.Posix                       hiding ((<.>))

import           Test.Hspec                                  (Spec, describe, it)

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
        it ("parsed" </> path) $
          safeIOToIO ((printIL <.> flipParseVisible TextLabel) <$> readStnFile path) `goldenShouldIO` buildAbsoluteWsIlFileName ("parsed" </> path)
        it ("optimized" </> path) $
          safeIOToIO ((printIL <.> optimize AllOptimizations <.> flipParseVisible TextLabel) <$> readStnFile path) `goldenShouldIO` buildAbsoluteWsIlFileName ("optimized" </> path)
