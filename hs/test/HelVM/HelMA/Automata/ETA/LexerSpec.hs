module HelVM.HelMA.Automata.ETA.LexerSpec (spec) where

import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Parser

import           HelVM.HelMA.Automata.ETA.FileExtra

import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.Optimizer

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           Control.Applicative.Tools

import           System.FilePath.Posix                       hiding ((<.>))

import           Test.Hspec                                  (Spec, describe, it)

spec :: Spec
spec =
  describe "lexer" $ forM_ allFiles $ \(fileName , dirName) -> do
      let path = dirName </> fileName
      let file = readEtaFile path
      it ("minified" </> path) $
        (show . readTokens <$> file) `goldenShouldIO` buildAbsoluteEtaFileName ("minified" </> path)
      it ("parsed" </> path) $
        safeIOToPTextIO (parseSafe <$> file) `goldenShouldIO` buildAbsoluteEtaIlFileName ("parsed" </> path)
      it ("optimized" </> path) $
        safeIOToPTextIO ((optimize AllOptimizations <.> parseSafe) <$> file) `goldenShouldIO` buildAbsoluteEtaIlFileName ("optimized" </> path)

allFiles :: [(FilePath, FilePath)]
allFiles = original <> fromEAS

original :: [(FilePath, FilePath)]
original =
  [ "hello"
  , "hello2"
  , "pip"
  , "pip2"
  , "fact"
  , "bottles"
  , "crlf"
  ] |><| ["original"]

fromEAS :: [(FilePath, FilePath)]
fromEAS =
  [ "true"
  , "hello"
  , "pip"
  , "pip2"
  , "reverse"
  , "function"
  , "writestr"
  , "hello2"
  , "hello3"
  , "hello4"
  , "writenum"
  , "multiply"
  , "readnum"
  , "fact"
  , "bottles"
  , "euclid"
  ] |><| [ "from-eas" ]
