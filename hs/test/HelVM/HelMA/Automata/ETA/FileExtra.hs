module HelVM.HelMA.Automata.ETA.FileExtra (
  readEtaFile,
  buildAbsoluteMinifiedEtaFileName,
  buildAbsoluteEtaFileName,
  buildAbsoluteEtaIlFileName,
  buildAbsoluteEtaOutFileName,
  buildAbsoluteEtaLogFileName,
  showCompile,
  options,
) where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes

readEtaFile :: FilePath -> IO Source
readEtaFile = readSourceFile . buildAbsoluteEtaFileName

buildAbsoluteMinifiedEtaFileName :: FilePath -> FilePath
buildAbsoluteMinifiedEtaFileName = buildAbsoluteModeFileName "minified" lang

buildAbsoluteEtaFileName :: FilePath -> FilePath
buildAbsoluteEtaFileName = buildAbsoluteLangFileName lang

buildAbsoluteEtaIlFileName :: FilePath -> FilePath
buildAbsoluteEtaIlFileName = buildAbsoluteIlFileName lang

buildAbsoluteEtaOutFileName :: FilePath -> FilePath
buildAbsoluteEtaOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteEtaLogFileName :: FilePath -> FilePath
buildAbsoluteEtaLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "eta"

showCompile :: Bool -> FilePath
showCompile False = "token"
showCompile True  = "instruction"
