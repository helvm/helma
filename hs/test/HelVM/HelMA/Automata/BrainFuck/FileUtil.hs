module HelVM.HelMA.Automata.BrainFuck.FileUtil (
  readBfFile,
  buildAbsoluteBfFileName,
  buildAbsoluteBfOutFileName,
  buildAbsoluteBfLogFileName,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

readBfFile :: FilePath -> IO Source
readBfFile = readFileText . buildAbsoluteBfFileName

buildAbsoluteBfFileName :: FilePath -> FilePath
buildAbsoluteBfFileName = buildAbsoluteLangFileName lang

buildAbsoluteBfOutFileName :: FilePath -> FilePath
buildAbsoluteBfOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteBfLogFileName :: FilePath -> FilePath
buildAbsoluteBfLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "bf"
