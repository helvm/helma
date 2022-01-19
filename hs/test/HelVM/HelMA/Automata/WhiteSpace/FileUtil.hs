module HelVM.HelMA.Automata.WhiteSpace.FileUtil (
  readWsFile,
  readStnFile,
  buildAbsoluteWsFileName,
  buildAbsoluteStnFileName,
  buildAbsoluteWsIlFileName,
  buildAbsoluteWsOutFileName,
  buildAbsoluteWsLogFileName,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

readWsFile :: FilePath -> IO Source
readWsFile = readFileText . buildAbsoluteWsFileName

readStnFile :: FilePath -> IO Source
readStnFile = readFileText . buildAbsoluteStnFileName

buildAbsoluteWsFileName :: FilePath -> FilePath
buildAbsoluteWsFileName = buildAbsoluteExtFileName lang lang

buildAbsoluteStnFileName :: FilePath -> FilePath
buildAbsoluteStnFileName = buildAbsoluteExtFileName stn lang

buildAbsoluteWsIlFileName :: FilePath -> FilePath
buildAbsoluteWsIlFileName = buildAbsoluteIlFileName lang

buildAbsoluteWsOutFileName :: FilePath -> FilePath
buildAbsoluteWsOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteWsLogFileName :: FilePath -> FilePath
buildAbsoluteWsLogFileName = buildAbsoluteLogFileName lang

stn :: FilePath
stn = "stn"

lang :: FilePath
lang = "ws"

