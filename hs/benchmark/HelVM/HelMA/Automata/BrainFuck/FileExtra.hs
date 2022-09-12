module HelVM.HelMA.Automata.BrainFuck.FileExtra (
  readBfFile,

  buildAbsoluteExecFileName,
  options,
) where

import           HelVM.HelMA.Automata.FileExtra

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
