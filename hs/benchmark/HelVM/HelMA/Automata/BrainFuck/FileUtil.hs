module HelVM.HelMA.Automata.BrainFuck.FileUtil (
  readBfFile,
  readOutFile,
  buildAbsoluteOutFileName
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readBfFile :: FilePath -> IO Source
readBfFile fileName = readFileText $ buildAbsoluteBfFileName fileName

readOutFile :: FilePath -> IO Source
readOutFile fileName = readFileText $ buildAbsoluteOutFileName fileName

buildAbsoluteBfFileName :: FilePath -> FilePath
buildAbsoluteBfFileName fileName = bfDir </> fileName <.> "bf"

buildAbsoluteOutFileName :: FilePath -> FilePath
buildAbsoluteOutFileName fileName = bfDir </> "output" </> fileName <.> "out"

bfDir :: FilePath
bfDir = dir </> "bf"

dir :: FilePath
dir = "examples"
