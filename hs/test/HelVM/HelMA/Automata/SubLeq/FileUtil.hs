module HelVM.HelMA.Automata.SubLeq.FileUtil (
  readSqFile,
  buildAbsoluteOutFileName
) where

import System.FilePath.Posix

readSqFile :: String -> IO String
readSqFile fileName = readFile $ buildAbsoluteSqFileName fileName

buildAbsoluteSqFileName :: String -> String
buildAbsoluteSqFileName fileName = sqDir </> fileName <.> "sq"

buildAbsoluteOutFileName :: String -> String
buildAbsoluteOutFileName fileName = sqDir </> "output" </> fileName <.> "out"

sqDir :: String
sqDir = dir </> "sq"

dir :: String
dir = "examples"
