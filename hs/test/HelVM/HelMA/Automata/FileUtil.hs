module HelVM.HelMA.Automata.FileUtil (
  readSourceFile,
  buildAbsoluteLangFileName,
  buildAbsoluteModeFileName,
  buildAbsoluteIlFileName,
  buildAbsoluteExtFileName,
  buildAbsoluteOutFileName,
  buildAbsoluteLogFileName,
  buildAbsoluteEvalFileName,
  showAscii,
  options,
) where

import           System.FilePath.Posix

readSourceFile :: MonadIO m => FilePath -> m Text
readSourceFile filePath = readFileText $ "examples" </> filePath

buildAbsoluteLangFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLangFileName lang fileName = lang </> fileName <.> lang

buildAbsoluteModeFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteModeFileName mode lang fileName = lang </> mode </> fileName <.> lang

buildAbsoluteIlFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteIlFileName = buildAbsoluteExtFileName "il"

buildAbsoluteExtFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteExtFileName ext lang fileName = lang </> ext </> fileName <.> ext

buildAbsoluteOutFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteOutFileName = buildAbsoluteEvalFileName "output"

buildAbsoluteLogFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLogFileName = buildAbsoluteEvalFileName "logged"

buildAbsoluteEvalFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteEvalFileName mode lang fileName = lang </> "eval" </> mode </> fileName <.> mode

showAscii:: Bool -> FilePath
showAscii False = "asciiOff"
showAscii True  = "asciiOn"

options :: [Bool]
options = [True , False]
