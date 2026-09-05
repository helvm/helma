module HelVM.HelMA.Automata.ETA.LexerSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.ETA.Evaluator

import           HelVM.HelMA.Automata.ETA.FileExtra

import           HelVM.HelIO.CartesianProduct
import           HelVM.HelIO.Control.Safe

import           HelVM.GoldenExpectations

import           System.FilePath.Posix              hiding ( (<.>) )

import           Test.Hspec                         ( Spec, describe, it )

spec ∷ Spec
spec =
  describe "lexer" $ forM_ allFiles $ \(fileName , dirName) -> do
      let path = dirName </> fileName
      let file = readEtaFile path
      it ("minified" </> path) $
        (emitCode <$> file) `goldenLShouldIO` buildAbsoluteEtaFileName ("minified" </> path)
      it ("parsed" </> path) $
        safeIOToIO (emitIL <$> file) `goldenLShouldIO` buildAbsoluteEtaIlFileName ("parsed" </> path)
      it ("optimized" </> path) $
        safeIOToIO (emitOptimizedIL <$> file) `goldenLShouldIO` buildAbsoluteEtaIlFileName ("optimized" </> path)

allFiles ∷ [(FilePath, FilePath)]
allFiles = original <> fromEAS

original ∷ [(FilePath, FilePath)]
original =
  [ "hello"
  , "hello2"
  , "pip"
  , "pip2"
  , "fact"
  , "bottles"
  , "crlf"
  ] >*< ["original"]

fromEAS ∷ [(FilePath, FilePath)]
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
  ] >*< [ "from-eas" ]
