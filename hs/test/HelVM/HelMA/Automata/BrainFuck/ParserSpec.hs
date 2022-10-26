module HelVM.HelMA.Automata.BrainFuck.ParserSpec (spec) where

import qualified HelVM.HelMA.Automata.BrainFuck.Fast.Parser as Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Flat.Parser as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Tree.Parser as Tree

import           HelVM.HelMA.Automata.BrainFuck.FileExtra

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                 (Spec, describe, it)

spec :: Spec
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
        it ("fast" </> fileName) $
          safeIOToPTextIO (Fast.parseAsVector <$> file) `goldenShouldIO` buildAbsoluteBfIlFileName ("fast" </> fileName)
        it ("tree" </> fileName) $
          safeIOToPTextIO (Tree.parseAsVector <$> file) `goldenShouldIO` buildAbsoluteBfIlFileName ("tree" </> fileName)
        it ("flat" </> fileName) $
           (showP . Flat.tokenize <$> file) `goldenShouldIO` buildAbsoluteBfIlFileName ("flat" </> fileName)
