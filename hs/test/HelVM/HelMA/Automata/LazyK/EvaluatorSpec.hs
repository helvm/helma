module HelVM.HelMA.Automata.LazyK.EvaluatorSpec where

import           HelVM.HelMA.Automata.LazyK.Evaluator
import           HelVM.HelMA.Automata.LazyK.FileExtra

import           HelVM.HelMA.Automaton.IO.MockIO

import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

--import           Data.Char

import           System.FilePath.Posix

import           Test.Hspec                           (Spec, describe, it)

spec :: Spec
spec =
  describe "eval" $
    describe "original" $ forM_ (
--      [
      [ "echo"
--      , "hello"
      ] |><| ["irori"]
      <>
--      [ "delete_blank_lines"
--      , "even_lines"
--      [ "fibonacci"
--      , "fizz_buzz"
--      , "hello_world_iota"
      [ "hello_world"
--      [ "hello_world_sk"
--    , "permutater"
      , "quine"
--      , "sort_characters"
      , "ultimate_problem"
      , "v"
      ] |><| ["rst76"]
      <>
      [
--      [ "ab"
--      , "befunge"
--      , "bwt"
--      , "calc"
--      , "fib"
--      , "iota-in-iota"
--      , "power2"
--      , "primes"
--        [ "quine"
--      , "reverse"
--      , "rot13"
--      , "sort"
--      , "unlambda"
      ] |><| ["tromp"]
      ) $ \(fileName , dirName) -> do
        let filePath = "original" </> dirName </> fileName
        let file = readLazyKFile filePath
        let input = ""
        let path = filePath <> toString input
        let mock = ioExecMockIOWithInput input . evalSource =<< file
        describe path $ do
          it ("output" </> path) $
            calculateOutput <$> mock `goldenShouldIO` buildAbsoluteLazyKOutFileName path
          it ("logged" </> path) $
            calculateLogged <$> mock `goldenShouldIO` buildAbsoluteLazyKLogFileName path
