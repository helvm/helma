module HelVM.HelMA.Automata.LazyK.ParserSpec (spec) where

import           HelVM.HelMA.Automata.LazyK.FileUtil
import           HelVM.HelMA.Automata.LazyK.Parser

import           HelVM.HelMA.Automata.LazyK.API.ParserType

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           Data.Char

import           System.FilePath.Posix

import           Test.Hspec                                (Spec, describe, it)

spec :: Spec
spec =
  describe "parse" $ do
    describe "original" $ forM_ (
      [ "ab"
      , "befunge"
      , "bwt"
      , "calc"
      , "fib"
      , "iota-in-iota"
      , "powers2"
      , "primes"
      , "quine"
      , "reverse"
      , "rot13"
      , "sort"
      , "unlambda"
      ] |><| ["tromp"]
      <>
      [ "delete_blank_lines"
      , "even_lines"
      , "fibonacci"
      , "fizz_buzz"
--      , "hello_world_iota"
      , "hello_world"
      , "hello_world_sk"
      , "permutater"
      , "quine"
      , "sort_characters"
      , "ultimate_problem"
      , "v"
      ] |><| ["rst76"]
      <>
      [ "echo"
      , "hello"
      ] |><| ["irori"]
      ) $ \ (fileName , dirName) -> do
      let filePath = "original" </> dirName </> fileName
      let file = readLazyKFile filePath
      it filePath $
        safeIOToPTextIO (parse defaultParserType <$> file) `goldenShouldIO` buildAbsoluteLazyKLambdaFileName filePath
    describe "generated" $ forM_ (
      [ "ab"
      , "fib"
      , "powers2"
      , "reverse"
      ] |><| homogeneousParserTypes
      ) $ \ (fileName , parseType) -> do
        let filePath = "generated" </> "tromp" </> (toLower <$> (show parseType)) </> fileName
        let file = readLazyKFile filePath
        it filePath $
          safeIOToPTextIO (parse parseType <$> file) `goldenShouldIO` buildAbsoluteLazyKLambdaFileName filePath
