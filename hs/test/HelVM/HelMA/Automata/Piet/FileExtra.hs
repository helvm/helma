module HelVM.HelMA.Automata.Piet.FileExtra where

import           HelVM.HelMA.Automata.FileExtra

import qualified Codec.Picture                  as Picture

import qualified RIO

import           System.FilePath.Posix

readImage :: FilePath -> IO Picture.DynamicImage
readImage path = Picture.readImage path >>= either RIO.throwString pure

buildAbsolutePietOutFileName :: FilePath -> FilePath
buildAbsolutePietOutFileName path = "piet" </> "output" </> path <.> "output"

buildAbsolutePietLogFileName :: FilePath -> FilePath
buildAbsolutePietLogFileName path = "piet" </> "logged" </> path <.> "log"

buildAbsolutePietFileName :: FilePath -> FilePath
buildAbsolutePietFileName path = examplesDir </> lang </> path

buildAbsoluteParsedFileName :: FilePath -> FilePath
buildAbsoluteParsedFileName = buildAbsoluteExtFileName "parsed" lang

lang :: FilePath
lang = "piet"
