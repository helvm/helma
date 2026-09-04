module HelVM.HelMA.Automata.Piet.FileExtra where

import           HelVM.HelMA.Automata.FileExtra

import qualified Codec.Picture                  as Picture

import qualified RIO

import           System.FilePath.Posix

readImage ∷ FilePath → IO Picture.DynamicImage
readImage path = Picture.readImage path >>= either RIO.throwString pure

buildAbsolutePietOutFileName ∷ FilePath → FilePath
buildAbsolutePietOutFileName path = "piet" </> "eval" </> "output" </> path <.> "output"

buildAbsolutePietLogFileName ∷ FilePath → FilePath
buildAbsolutePietLogFileName path = "piet" </> "eval" </> "logged" </> path <.> "log"

buildAbsolutePietIlFileName ∷ FilePath → FilePath
buildAbsolutePietIlFileName path = "piet" </> "il" </> path <.> "il"

buildAbsolutePietTlFileName ∷ FilePath → FilePath
buildAbsolutePietTlFileName path = "piet" </> "tl" </> path <.> "tl"

buildAbsolutePietDotFileName ∷ FilePath → FilePath
buildAbsolutePietDotFileName path = "piet" </> "dot" </> path <.> "dot"

buildAbsolutePietFileName ∷ FilePath → FilePath
buildAbsolutePietFileName path = examplesDir </> lang </> path

buildAbsoluteParsedFileName ∷ FilePath → FilePath
buildAbsoluteParsedFileName = buildAbsoluteExtFileName "parsed" lang

lang ∷ FilePath
lang = "piet"
