module HelVM.HelMA.Automata.BrainFuck.FileUtil (
  readBfFile,
  readOutFile,
  buildAbsoluteOutFileName
) where

import System.FilePath.Posix

readBfFile :: String -> IO String
readBfFile fileName = readFile $ buildAbsoluteBfFileName fileName

readOutFile :: String -> IO String
readOutFile fileName = readFile $ buildAbsoluteOutFileName fileName

buildAbsoluteBfFileName :: String -> String
buildAbsoluteBfFileName fileName = bfDir </> fileName <.> "bf"

buildAbsoluteOutFileName :: String -> String
buildAbsoluteOutFileName fileName = bfDir </> "output" </> fileName <.> "out"

bfDir :: String
bfDir = dir </> "bf"

dir :: String
dir = "examples"
