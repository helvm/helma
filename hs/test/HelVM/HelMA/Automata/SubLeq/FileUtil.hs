module HelVM.HelMA.Automata.SubLeq.FileUtil (
  readSqFile,
  buildAbsoluteSqFileName,
  buildAbsoluteSqOutFileName,
  buildAbsoluteSqLogFileName,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

readSqFile :: FilePath -> IO Source
readSqFile = readSourceFile . buildAbsoluteSqFileName

buildAbsoluteSqFileName :: FilePath -> FilePath
buildAbsoluteSqFileName = buildAbsoluteLangFileName lang

buildAbsoluteSqOutFileName :: FilePath -> FilePath
buildAbsoluteSqOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteSqLogFileName :: FilePath -> FilePath
buildAbsoluteSqLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "sq"
