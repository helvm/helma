module HelVM.HelMA.Automata.LazyK.Rst76AutomatonSpec where

import           HelVM.HelMA.Automata.LazyK.FileUtil
import           HelVM.HelMA.Automata.LazyK.Rst76.Automaton

import           HelVM.HelMA.Automaton.IO.MockIO

import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                 (Spec, describe, it)

spec :: Spec
spec =
  describe "run" $ forM_ (
    [
--    [ "echo"
--    , "hello"
    ] |><| ["irori"]
    <>
--    [ "delete_blank_lines"
--    , "even_lines"
--    [ "fibonacci"
--    , "fizz_buzz"
--    , "hello_world_iota"
      [ "hello_world"
--    [ "hello_world_sk"
--    , "permutater"
    , "quine"
--    , "sort_characters"
    , "ultimate_problem"
    , "v"
    ] |><| ["rst76"]
    <>
    [
--    [ "ab"
--    , "befunge"
--    , "bwt"
--    , "calc"
--    , "fib"
--    , "iota-in-iota"
--    , "power2"
--    , "primes"
--      [ "quine"
--    , "reverse"
--    , "rot13"
--    , "sort"
--    , "unlambda"
    ] |><| ["tromp"]
    ) $ \(fileName , dirName) -> do
    let filePath = "original" </> dirName </> fileName
    let file = readLazyKFile filePath
    let input = ""
    let path = "rst76" </> filePath <> toString input
    let mock = ioExecMockIOWithInput input . run =<< file
    describe path $ do
      it ("output" </> path) $
        calculateOutput <$> mock `goldenShouldIO` buildAbsoluteLazyKOutFileName path
      it ("logged" </> path) $
        calculateLogged <$> mock `goldenShouldIO` buildAbsoluteLazyKLogFileName path
