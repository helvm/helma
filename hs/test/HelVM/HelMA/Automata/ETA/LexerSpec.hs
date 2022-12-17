module HelVM.HelMA.Automata.ETA.LexerSpec (spec) where

import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Parser

import           HelVM.HelMA.Automata.ETA.FileExtra

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                         (Spec, describe, it)

spec :: Spec
spec =
  describe "lexer" $ do
    describe "minified" $ forM_ (original <> fromEAS) $ \(fileName , dirName) -> do
      let path = dirName </> fileName
      let file = readEtaFile path
      it ("minified" </> path) $
        (show . readTokens <$> file) `goldenShouldIO` buildAbsoluteEtaFileName ("minified" </> path)
    describe "parsed" $ forM_ fromEAS $ \(fileName , dirName) -> do
      let path = dirName </> fileName
      let file = readEtaFile path
      it ("parsed" </> path) $
        safeIOToPTextIO (parseSafe <$> file) `goldenShouldIO` buildAbsoluteEtaIlFileName ("parsed" </> path)

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
