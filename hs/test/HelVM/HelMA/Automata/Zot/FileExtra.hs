module HelVM.HelMA.Automata.Zot.FileExtra
  ( binaryOnly
  , buildAbsoluteZotFileName
  , buildAbsoluteZotLogFileName
  , buildAbsoluteZotOutFileName
  , options
  , readZotFile
  , showAscii
  ) where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.API.LabelType

readZotFile ∷ FilePath → IO Source
readZotFile = readSourceFile . buildAbsoluteZotFileName

buildAbsoluteZotFileName ∷ FilePath → FilePath
buildAbsoluteZotFileName = buildAbsoluteLangFileName lang

buildAbsoluteZotOutFileName ∷ FilePath → FilePath
buildAbsoluteZotOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteZotLogFileName ∷ FilePath → FilePath
buildAbsoluteZotLogFileName = buildAbsoluteLogFileName lang

lang ∷ FilePath
lang = "zot"

binaryOnly ∷ [LabelType]
binaryOnly = [BinaryLabel]
