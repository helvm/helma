module HelVM.HelMA.Automata.Piet.FileExtra where

import           HelVM.HelMA.Automata.FileExtra

import           System.FilePath.Posix

buildAbsolutePietFileName :: FilePath -> FilePath
buildAbsolutePietFileName path = examplesDir </> lang </> path

buildAbsoluteParsedFileName :: FilePath -> FilePath
buildAbsoluteParsedFileName = buildAbsoluteExtFileName "parsed" lang

lang :: FilePath
lang = "piet"
