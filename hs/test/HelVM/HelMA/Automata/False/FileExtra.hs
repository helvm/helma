module HelVM.HelMA.Automata.False.FileExtra where

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readFFile :: FilePath -> IO Source
readFFile fileName = readFileText $ buildAbsoluteFFileName fileName

buildAbsoluteFFileName :: FilePath -> FilePath
buildAbsoluteFFileName fileName = fDir </> fileName <.> "f"

buildAbsoluteOutFileName :: FilePath -> FilePath
buildAbsoluteOutFileName fileName = fDir </> "output" </> fileName <.> "out"

buildAbsoluteIlFileName :: FilePath -> FilePath
buildAbsoluteIlFileName fileName = fDir </> "il" </> fileName <.> "il"

fDir :: FilePath
fDir = dir </> "f"

dir :: FilePath
dir = "examples"
