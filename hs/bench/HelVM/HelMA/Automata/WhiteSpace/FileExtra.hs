module HelVM.HelMA.Automata.WhiteSpace.FileExtra
  ( buildAbsoluteExecFileName
  , buildAbsoluteIlFileName
  , buildAbsoluteStnFileName
  , buildAbsoluteWsFileName
  , options
  , readStnFile
  , readWsFile
  ) where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes

import           System.FilePath.Posix

readWsFile ∷ FilePath → IO Source
readWsFile = readSourceFile . buildAbsoluteWsFileName

readStnFile ∷ FilePath → IO Source
readStnFile = readSourceFile . buildAbsoluteStnFileName

buildAbsoluteWsFileName ∷ FilePath → FilePath
buildAbsoluteWsFileName fileName = wsDir </> "ws" </> fileName <.> "ws"

buildAbsoluteStnFileName ∷ FilePath → FilePath
buildAbsoluteStnFileName fileName = wsDir </> "stn" </> fileName <.> "stn"

buildAbsoluteIlFileName ∷ FilePath → FilePath
buildAbsoluteIlFileName fileName = wsDir </> "il" </> fileName <.> "il"

buildAbsoluteExecFileName ∷ FilePath → FilePath
buildAbsoluteExecFileName fileName = wsDir </> "eval" </> fileName

wsDir ∷ FilePath
wsDir = "ws"
