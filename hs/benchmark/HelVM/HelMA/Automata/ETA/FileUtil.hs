module HelVM.HelMA.Automata.ETA.FileUtil (
  readEtaFile,
  buildAbsoluteEtaFileName,
  buildAbsoluteExecFileName,
  options,
) where

import           HelVM.HelMA.Automata.FileUtil

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readEtaFile :: FilePath -> IO Source
readEtaFile = readSourceFile . buildAbsoluteEtaFileName

buildAbsoluteEtaFileName :: FilePath -> FilePath
buildAbsoluteEtaFileName fileName = etaDir </> fileName <.> "eta"

buildAbsoluteExecFileName :: FilePath -> FilePath
buildAbsoluteExecFileName fileName = etaDir </> "eval" </> fileName

etaDir :: FilePath
etaDir = "eta"
