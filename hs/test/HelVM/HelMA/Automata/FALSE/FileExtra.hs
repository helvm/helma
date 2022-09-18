module HelVM.HelMA.Automata.FALSE.FileExtra where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes

readFFile :: FilePath -> IO Source
readFFile = readSourceFile . buildAbsoluteFFileName

buildAbsoluteFFileName :: FilePath -> FilePath
buildAbsoluteFFileName = buildAbsoluteLangFileName lang

buildAbsoluteFIlFileName :: FilePath -> FilePath
buildAbsoluteFIlFileName = buildAbsoluteIlFileName lang

buildAbsoluteFOutFileName :: FilePath -> FilePath
buildAbsoluteFOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteFLogFileName :: FilePath -> FilePath
buildAbsoluteFLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "f"
