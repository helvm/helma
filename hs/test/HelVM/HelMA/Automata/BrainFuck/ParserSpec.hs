module HelVM.HelMA.Automata.BrainFuck.ParserSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.BrainFuck.API.ImplType
import           HelVM.HelMA.Automata.BrainFuck.Evaluator

import           HelVM.HelMA.Automata.BrainFuck.FileExtra

import           HelVM.HelIO.Control.Safe

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                  ( Spec, describe, it )

spec ∷ Spec
spec =
  describe "parse" $ forM_
    [ "value256"
    , "helloWorld"
    , "fascistHelloWorld"
    , "padHelloWorld"
    , "theShortestHelloWorld"
    , "99botles"
    , "triangle"
    , "fibonacci"
    ]
    $ \ fileName -> do
      let file = readBfFile fileName
      describe fileName $ do
        it ("optimized" </> fileName) $
          safeIOToIO (emitIL FastType True  <$> file) `goldenLShouldIO` buildAbsoluteBfIlFileName ("optimized" </> fileName)
        it ("fast" </> fileName) $
          safeIOToIO (emitIL FastType False <$> file) `goldenLShouldIO` buildAbsoluteBfIlFileName ("fast" </> fileName)
        it ("tree" </> fileName) $
          safeIOToIO (emitIL TreeType False <$> file) `goldenLShouldIO` buildAbsoluteBfIlFileName ("tree" </> fileName)
        it ("flat" </> fileName) $
          safeIOToIO (emitIL FlatType False <$> file) `goldenLShouldIO` buildAbsoluteBfIlFileName ("flat" </> fileName)
