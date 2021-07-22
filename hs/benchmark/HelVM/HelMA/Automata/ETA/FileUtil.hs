module HelVM.HelMA.Automata.ETA.FileUtil (
  readEtaFile,
  readOutFile,
  buildAbsoluteEtaFileName,
  buildAbsoluteOutFileName
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readEtaFile :: FilePath -> IO Source
readEtaFile = readFileText . buildAbsoluteEtaFileName

readOutFile :: FilePath -> IO Source
readOutFile = readFileText . buildAbsoluteOutFileName

buildAbsoluteEtaFileName :: FilePath -> FilePath
buildAbsoluteEtaFileName fileName = etaDir </> fileName <.> "eta"

buildAbsoluteOutFileName :: FilePath -> FilePath
buildAbsoluteOutFileName fileName = etaDir </> "output" </> fileName <.> "out"

etaDir :: FilePath
etaDir = dir </> "eta"

dir :: FilePath
dir = "examples"
