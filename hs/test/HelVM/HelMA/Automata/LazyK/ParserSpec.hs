module HelVM.HelMA.Automata.LazyK.ParserSpec (spec) where

import           HelVM.HelMA.Automata.LazyK.FileExtra
import           HelVM.HelMA.Automata.LazyK.Parser

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                           (Spec, describe, it)

spec :: Spec
spec =
  describe "parse" $
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
      ] >*< ["tromp"]
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
      ] >*< ["rst76"]
      <>
      [ "echo"
      , "hello"
      ] >*< ["irori"]
      ) $ \ (fileName , dirName) -> do
      let filePath = "original" </> dirName </> fileName
      let file = readLazyKFile filePath
      it filePath $
        safeIOToPTextIO (parse <$> file) `goldenShouldIO` buildAbsoluteLazyKLambdaFileName filePath
