module HelVM.HelMA.Automata.Piet.LLVM.TestUtils
  ( toVector2D
  , withTempFile
  ) where

import           Control.Exception
import           Data.Vector         ( Vector )
import qualified Data.Vector.Generic as V
import           System.Directory
import           System.IO

toVector2D ∷ [[a]] → Vector (Vector a)
toVector2D = V.fromList . fmap V.fromList

withTempFile ∷ String → (FilePath → IO a) → IO a
withTempFile template = bracket createTempFile removeTempFile where
  createTempFile = do
    tempDir <- getTemporaryDirectory
    (path, fHandle) <- openTempFile tempDir template
    hClose fHandle
    return path
  removeTempFile = removeFile
