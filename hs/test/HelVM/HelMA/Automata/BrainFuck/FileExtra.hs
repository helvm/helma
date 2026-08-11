module HelVM.HelMA.Automata.BrainFuck.FileExtra
  ( buildAbsoluteBfFileName
  , buildAbsoluteBfIlFileName
  , buildAbsoluteBfLogFileName
  , buildAbsoluteBfOutFileName
  , readBfFile
  ) where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes

readBfFile ∷ FilePath → IO Source
readBfFile = readSourceFile . buildAbsoluteBfFileName

buildAbsoluteBfFileName ∷ FilePath → FilePath
buildAbsoluteBfFileName = buildAbsoluteLangFileName lang

buildAbsoluteBfIlFileName ∷ FilePath → FilePath
buildAbsoluteBfIlFileName = buildAbsoluteIlFileName lang

buildAbsoluteBfOutFileName ∷ FilePath → FilePath
buildAbsoluteBfOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteBfLogFileName ∷ FilePath → FilePath
buildAbsoluteBfLogFileName = buildAbsoluteLogFileName lang

lang ∷ FilePath
lang = "bf"
