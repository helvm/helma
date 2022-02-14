module HelVM.HelMA.Automata.Zot.EvaluatorSpec where

import           HelVM.HelMA.Automata.Zot.Evaluator
import           HelVM.HelMA.Automata.Zot.FileUtil

import           HelVM.Common.Control.Safe
import           HelVM.Common.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                         (Spec, describe, it)

spec :: Spec
spec = describe "eval" $ forM_ ((
  [ ("hello"             , ""         , options)
  , ("reverse"           , "10101010" , [False])
  , ("reverse"           , "01010101" , [False])
  ] |><| ["original"]
  ) <> (
  [ ("flipPrint" , "10101010" , options)
  , ("id"        , "10101010" , options)
  , ("print3"    , "10101010" , options)
--  , ("rev"       , "10101010" , options)
  , ("reverse"   , "10101010" , options)
  ] |><| ["lambda"]
  )) $ \((fileName , input , asciis) , dirName) -> do
  let filePath = dirName </> fileName
  let file = readZotFile filePath
  forM_ asciis $ \ ascii -> do
    let path = showAscii ascii </> filePath <> toString input
    describe path $
      it ("output" </> path) $
        safeIOToIO (flip (eval ascii) input <$> file) `goldenShouldIO` buildAbsoluteZotOutFileName path
