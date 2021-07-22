module HelVM.HelMA.Automata.SubLeq.FileUtil (
  readSqFile,
  buildAbsoluteOutFileName
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readSqFile :: FilePath -> IO Source
readSqFile fileName = readFileText $ buildAbsoluteSqFileName fileName

buildAbsoluteSqFileName :: FilePath -> FilePath
buildAbsoluteSqFileName fileName = sqDir </> fileName <.> "sq"

buildAbsoluteOutFileName :: FilePath -> FilePath
buildAbsoluteOutFileName fileName = sqDir </> "output" </> fileName <.> "out"

sqDir :: FilePath
sqDir = dir </> "sq"

dir :: FilePath
dir = "examples"
