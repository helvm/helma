module HelVM.HelMA.Automata.Zot.FileUtil (
  readZotFile,
  buildAbsoluteZotFileName,
  buildAbsoluteZotOutFileName,
  buildAbsoluteZotLogFileName,
  showAscii,
  binaryOnly,
  options,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

readZotFile :: FilePath -> IO Source
readZotFile = readSourceFile . buildAbsoluteZotFileName

buildAbsoluteZotFileName :: FilePath -> FilePath
buildAbsoluteZotFileName = buildAbsoluteLangFileName lang

buildAbsoluteZotOutFileName :: FilePath -> FilePath
buildAbsoluteZotOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteZotLogFileName :: FilePath -> FilePath
buildAbsoluteZotLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "zot"

binaryOnly :: [Bool]
binaryOnly = [False]
