module HelVM.HelMA.Automata.BrainFuck.FileUtil (
  readBfFile,
  readOutFile,
  buildAbsoluteExecFileName
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readBfFile :: FilePath -> IO Source
readBfFile fileName = readFileText $ buildAbsoluteBfFileName fileName

readOutFile :: FilePath -> IO Source
readOutFile fileName = readFileText $ buildAbsoluteExecFileName fileName

buildAbsoluteBfFileName :: FilePath -> FilePath
buildAbsoluteBfFileName fileName = bfDir </> fileName <.> "bf"

buildAbsoluteExecFileName :: FilePath -> FilePath
buildAbsoluteExecFileName fileName = bfDir </> "eval" </> fileName

bfDir :: FilePath
bfDir = dir </> "bf"

dir :: FilePath
dir = "examples"
