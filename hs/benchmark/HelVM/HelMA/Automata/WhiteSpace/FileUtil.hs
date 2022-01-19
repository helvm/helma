module HelVM.HelMA.Automata.WhiteSpace.FileUtil (
  readWsFile,
  readOutFile,
  readStnFile,
  buildAbsoluteWsFileName,
  buildAbsoluteStnFileName,
  buildAbsoluteIlFileName,
  buildAbsoluteExecFileName
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readWsFile :: FilePath -> IO Source
readWsFile fileName = readFileText $ buildAbsoluteWsFileName fileName

readStnFile :: FilePath -> IO Source
readStnFile fileName = readFileText $ buildAbsoluteStnFileName fileName

readOutFile :: FilePath -> IO Source
readOutFile fileName = readFileText $ buildAbsoluteExecFileName fileName

buildAbsoluteWsFileName :: FilePath -> FilePath
buildAbsoluteWsFileName fileName = wsDir </> "ws" </> fileName <.> "ws"

buildAbsoluteStnFileName :: FilePath -> FilePath
buildAbsoluteStnFileName fileName = wsDir </> "stn" </> fileName <.> "stn"

buildAbsoluteIlFileName :: FilePath -> FilePath
buildAbsoluteIlFileName fileName = wsDir </> "il" </> fileName <.> "il"

buildAbsoluteExecFileName :: FilePath -> FilePath
buildAbsoluteExecFileName fileName = wsDir </> "exec" </> fileName

wsDir :: FilePath
wsDir = dir </> "ws"

dir :: FilePath
dir = "examples"
