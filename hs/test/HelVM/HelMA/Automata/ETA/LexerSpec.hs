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
  describe "lexer" $ forM_ ((
    [ "hello"
    , "hello2"
    , "pip"
    , "pip2"
    , "fact"
    , "bottles"
    , "crlf"
    ] |><| ["original"]
    ) <> (
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
    )) $ \(fileName , dirName) -> do
    let path = dirName </> fileName
    let file = readEtaFile path
    describe path $ do
      it ("minified" </> path) $
        (show . readTokens <$> file) `goldenShouldIO` buildAbsoluteEtaFileName ("minified" </> path)
      it ("parsed" </> path) $
        safeIOToPTextIO (parseSafe <$> file) `goldenShouldIO` buildAbsoluteEtaIlFileName ("parsed" </> path)
