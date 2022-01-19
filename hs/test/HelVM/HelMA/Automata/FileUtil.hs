module HelVM.HelMA.Automata.FileUtil (
  buildAbsoluteLangFileName,
  buildAbsoluteModeFileName,
  buildAbsoluteIlFileName,
  buildAbsoluteExtFileName,
  buildAbsoluteOutFileName,
  buildAbsoluteLogFileName,
  buildAbsoluteExecFileName,
  dir,
) where

import           System.FilePath.Posix

buildAbsoluteLangFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLangFileName lang fileName = dir </> lang </> fileName <.> lang

buildAbsoluteModeFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteModeFileName mode lang fileName = dir </> lang </> mode </> fileName <.> lang

buildAbsoluteIlFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteIlFileName = buildAbsoluteExtFileName "il"

buildAbsoluteExtFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteExtFileName ext lang fileName = dir </> lang </> ext </> fileName <.> ext

buildAbsoluteOutFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteOutFileName = buildAbsoluteExecFileName "output"

buildAbsoluteLogFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLogFileName = buildAbsoluteExecFileName "logged"

buildAbsoluteExecFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteExecFileName mode lang fileName = dir </> lang </> "exec" </> mode </> fileName <.> mode

dir :: FilePath
dir = "examples"
