module HelVM.HelMA.Automata.WhiteSpace.FileUtil (
  readWsFile,
  readOutFile,
  readStnFile,
  buildAbsoluteWsFileName,
  buildAbsoluteStnFileName,
  buildAbsoluteIlFileName,
  buildAbsoluteOutFileName
) where

import System.FilePath.Posix

readWsFile :: String -> IO String
readWsFile fileName = readFile $ buildAbsoluteWsFileName fileName

readStnFile :: String -> IO String
readStnFile fileName = readFile $ buildAbsoluteStnFileName fileName

readOutFile :: String -> IO String
readOutFile fileName = readFile $ buildAbsoluteOutFileName fileName

buildAbsoluteWsFileName :: String -> String
buildAbsoluteWsFileName fileName = wsDir </> "ws" </> fileName <.> "ws"

buildAbsoluteStnFileName :: String -> String
buildAbsoluteStnFileName fileName = wsDir </> "stn" </> fileName <.> "stn"

buildAbsoluteIlFileName :: String -> String
buildAbsoluteIlFileName fileName = wsDir </> "il" </> fileName <.> "il"

buildAbsoluteOutFileName :: String -> String
buildAbsoluteOutFileName fileName = wsDir </> "output" </> fileName <.> "out"

wsDir :: String
wsDir = dir </> "ws/"

dir :: String
dir = "examples"
