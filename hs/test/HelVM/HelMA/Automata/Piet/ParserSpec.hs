module HelVM.HelMA.Automata.Piet.ParserSpec (spec) where

import           HelVM.HelMA.Automata.Piet.FileExtra
import           HelVM.HelMA.Automata.Piet.Parser

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                          (Spec, describe, it)

spec :: Spec
spec = describe "parser" $ forM_ allFilePaths $ \ path ->
  it path $ parseToRightTextIO (buildAbsolutePietFileName path) `goldenShouldIO` buildAbsoluteParsedFileName path

allFilePaths :: [FilePath]
allFilePaths =
  [ "pietcc" </> "hw" <.> "png"
  ]
