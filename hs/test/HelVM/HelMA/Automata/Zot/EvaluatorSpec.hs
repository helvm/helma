module HelVM.HelMA.Automata.Zot.EvaluatorSpec where

import           HelVM.HelMA.Automata.Zot.Automaton
import           HelVM.HelMA.Automata.Zot.FileExtra

import           HelVM.HelMA.Automaton.Types.FormatType

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                             (Spec, describe, it)

spec :: Spec
spec = describe "eval" $ forM_ ((
  [ ("hello"             , ""         , toList formatTypes)
  , ("reverse"           , "10101010" , binaryOnly)
  , ("reverse"           , "01010101" , binaryOnly)
  ] >*< ["original"]
  ) <> (
  [ ("flipPrint" , "10101010" , binaryOnly)
  , ("id"        , "10101010" , binaryOnly)
  , ("print3"    , "10101010" , binaryOnly)
--  , ("rev"       , "10101010" , binaryOnly)
  , ("reverse"   , "10101010" , binaryOnly)
  ] >*< ["calculus"]
  )) $ \((fileName , input , asciis) , dirName) -> do
  let filePath = dirName </> fileName
  let file = readZotFile filePath
  forM_ asciis $ \ ascii -> do
    let path = showAscii ascii </> filePath <> toString input
    describe path $
      it ("output" </> path) $
        safeIOToIO (flip (evalWithFormat ascii) input <$> file) `goldenShouldIO` buildAbsoluteZotOutFileName path
