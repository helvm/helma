module HelVM.HelMA.Automata.Piet.TestUtils
  ( toGrid
  , toVector2D
  , withTempFile
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Grid


import           Control.Exception

import qualified Data.Vector.Generic                  as V

import           System.Directory
import           System.IO

toGrid ∷ [[a]] → Grid a
toGrid = matrixToGrid . V.fromList . fmap V.fromList

toVector2D ∷ [[a]] → Matrix a
toVector2D = V.fromList . fmap V.fromList

withTempFile ∷ String → (FilePath → IO a) → IO a
withTempFile template = bracket createTempFile removeTempFile where
  createTempFile = do
    tempDir <- getTemporaryDirectory
    (path, fHandle) <- openTempFile tempDir template
    hClose fHandle
    pure path
  removeTempFile = removeFile
