module HelVM.HelMA.Automata.BrainFuck.FileUtil (
  readBfFile,

  buildAbsoluteExecFileName,
  options,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readBfFile :: FilePath -> IO Source
readBfFile = readSourceFile . buildAbsoluteBfFileName

buildAbsoluteBfFileName :: FilePath -> FilePath
buildAbsoluteBfFileName fileName = bfDir </> fileName <.> "bf"

buildAbsoluteExecFileName :: FilePath -> FilePath
buildAbsoluteExecFileName fileName = bfDir </> "eval" </> fileName

bfDir :: FilePath
bfDir = "bf"
