module HelVM.HelMA.Automata.SubLeq.FileUtil (
  readSqFile,
  buildAbsoluteExecFileName,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readSqFile :: FilePath -> IO Source
readSqFile = readSourceFile . buildAbsoluteSqFileName

buildAbsoluteSqFileName :: FilePath -> FilePath
buildAbsoluteSqFileName fileName = sqDir </> fileName <.> "sq"

buildAbsoluteExecFileName :: FilePath -> FilePath
buildAbsoluteExecFileName fileName = sqDir </> "eval" </> fileName

sqDir :: FilePath
sqDir = "sq"
