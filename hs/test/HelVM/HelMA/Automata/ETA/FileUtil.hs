module HelVM.HelMA.Automata.ETA.FileUtil (
  readEtaFile,
  buildAbsoluteMinifiedEtaFileName,
  buildAbsoluteEtaFileName,
  buildAbsoluteEtaOutFileName,
  buildAbsoluteEtaLogFileName,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

readEtaFile :: FilePath -> IO Source
readEtaFile = readFileText . buildAbsoluteEtaFileName

buildAbsoluteMinifiedEtaFileName :: FilePath -> FilePath
buildAbsoluteMinifiedEtaFileName = buildAbsoluteModeFileName "minified" lang

buildAbsoluteEtaFileName :: FilePath -> FilePath
buildAbsoluteEtaFileName = buildAbsoluteLangFileName lang

buildAbsoluteEtaOutFileName :: FilePath -> FilePath
buildAbsoluteEtaOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteEtaLogFileName :: FilePath -> FilePath
buildAbsoluteEtaLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "eta"
