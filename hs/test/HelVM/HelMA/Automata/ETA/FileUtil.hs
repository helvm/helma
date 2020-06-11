module HelVM.HelMA.Automata.ETA.FileUtil (
  readEtaFile,
  readOutFile,
  buildAbsoluteEtaFileName,
  buildAbsoluteOutFileName
) where

import System.FilePath.Posix

readEtaFile :: String -> IO String
readEtaFile fileName = readFile $ buildAbsoluteEtaFileName fileName

readOutFile :: String -> IO String
readOutFile fileName = readFile $ buildAbsoluteOutFileName fileName

buildAbsoluteEtaFileName :: String -> String
buildAbsoluteEtaFileName fileName = etaDir </> fileName <.> "eta"

buildAbsoluteOutFileName :: String -> String
buildAbsoluteOutFileName fileName = etaDir </> "output" </> fileName <.> "out"

etaDir :: String
etaDir = dir </> "eta/"

dir :: String
dir = "examples"
