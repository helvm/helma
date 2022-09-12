module HelVM.HelMA.Automata.BrainFuck.FileExtra (
  readBfFile,
  buildAbsoluteBfFileName,
  buildAbsoluteBfOutFileName,
  buildAbsoluteBfLogFileName,
  compileToFilePath,
  options,
) where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes

readBfFile :: FilePath -> IO Source
readBfFile = readSourceFile . buildAbsoluteBfFileName

buildAbsoluteBfFileName :: FilePath -> FilePath
buildAbsoluteBfFileName = buildAbsoluteLangFileName lang

buildAbsoluteBfOutFileName :: FilePath -> FilePath
buildAbsoluteBfOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteBfLogFileName :: FilePath -> FilePath
buildAbsoluteBfLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "bf"

compileToFilePath :: Bool -> FilePath
compileToFilePath False = "flat"
compileToFilePath True  = "tree"
