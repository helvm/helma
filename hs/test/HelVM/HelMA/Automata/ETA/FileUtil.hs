module HelVM.HelMA.Automata.ETA.FileUtil (
  readEtaFile,
  readOutFile,
  buildAbsoluteEtaFileName,
  buildAbsoluteOutFileName
) where

import HelVM.HelMA.Automaton.API.IOTypes

import System.FilePath.Posix

readEtaFile :: FilePath -> IO Source
readEtaFile fileName = readFileText $ buildAbsoluteEtaFileName fileName

readOutFile :: FilePath -> IO Source
readOutFile fileName = readFileText $ buildAbsoluteOutFileName fileName

buildAbsoluteEtaFileName :: FilePath -> FilePath
buildAbsoluteEtaFileName fileName = etaDir </> fileName <.> "eta"

buildAbsoluteOutFileName :: FilePath -> FilePath
buildAbsoluteOutFileName fileName = etaDir </> "output" </> fileName <.> "out"

etaDir :: FilePath
etaDir = dir </> "eta/"

dir :: FilePath
dir = "examples"
