module HelVM.HelMA.Automata.FileUtil (
  readSourceFile,
  options,
) where

import           System.FilePath.Posix

readSourceFile :: MonadIO m => FilePath -> m Text
readSourceFile filePath = readFileText $ "examples" </> filePath

options :: [Bool]
options = [False , True]
