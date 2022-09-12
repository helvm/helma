module HelVM.HelMA.Automata.SubLeq.FileExtra (
  readSqFile,
  buildAbsoluteSqFileName,
  buildAbsoluteSqOutFileName,
  buildAbsoluteSqLogFileName,
) where

import           HelVM.HelMA.Automata.FileExtra

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
