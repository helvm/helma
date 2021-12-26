module HelVM.HelMA.Automata.False.ParserSpec (spec) where

import           HelVM.HelMA.Automata.False.FileUtil
import           HelVM.HelMA.Automata.False.Parser

--import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           HelVM.HelIO.Control.Safe

--import           System.FilePath.Posix

import           Test.Hspec                          (Spec, describe, it)

spec :: Spec
spec = do
  describe "parse" $ do
    forM_ [ "strlen/contrib/Herb_Wollman/Fibonacci"
          ] $ \ fileName -> do
      it fileName $ do
        safeIOToPTextIO (parseFromTextSafe <$> readFFile fileName) `goldenShouldIO` buildAbsoluteIlFileName fileName
