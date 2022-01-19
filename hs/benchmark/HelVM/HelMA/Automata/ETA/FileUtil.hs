module HelVM.HelMA.Automata.ETA.FileUtil (
  readEtaFile,
  readOutFile,
  buildAbsoluteEtaFileName,
  buildAbsoluteExecFileName
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readEtaFile :: FilePath -> IO Source
readEtaFile = readFileText . buildAbsoluteEtaFileName

readOutFile :: FilePath -> IO Source
readOutFile = readFileText . buildAbsoluteExecFileName

buildAbsoluteEtaFileName :: FilePath -> FilePath
buildAbsoluteEtaFileName fileName = etaDir </> fileName <.> "eta"

buildAbsoluteExecFileName :: FilePath -> FilePath
buildAbsoluteExecFileName fileName = etaDir </> "exec" </> fileName

etaDir :: FilePath
etaDir = dir </> "eta"

dir :: FilePath
dir = "examples"
