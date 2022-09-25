module HelVM.HelMA.Automata.FileExtra (
  readSourceFile,
  buildAbsoluteLangFileName,
  buildAbsoluteModeFileName,
  buildAbsoluteLambdaFileName,
  buildAbsoluteIlFileName,
  buildAbsoluteExtFileName,
  buildAbsoluteOutFileName,
  buildAbsoluteLogFileName,
  buildAbsoluteEvalFileName,
  showAscii,
  options,
) where

import           HelVM.HelMA.Automaton.Types.FormatType

import           System.FilePath.Posix

readSourceFile :: MonadIO m => FilePath -> m Text
readSourceFile filePath = readFileText $ "examples" </> filePath

buildAbsoluteLangFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLangFileName lang fileName = lang </> fileName <.> lang

buildAbsoluteModeFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteModeFileName mode lang fileName = lang </> mode </> fileName <.> lang

buildAbsoluteIlFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteIlFileName = buildAbsoluteExtFileName "il"

buildAbsoluteLambdaFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLambdaFileName = buildAbsoluteExtFileName "lambda"

buildAbsoluteExtFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteExtFileName ext lang fileName = lang </> ext </> fileName <.> ext

buildAbsoluteOutFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteOutFileName = buildAbsoluteEvalFileName "output"

buildAbsoluteLogFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLogFileName = buildAbsoluteEvalFileName "logged"

buildAbsoluteEvalFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteEvalFileName mode lang fileName = lang </> "eval" </> mode </> fileName <.> mode

--FIXME
showAscii:: FormatType -> FilePath
showAscii BinaryLabel = "asciiOff"
showAscii TextLabel   = "asciiOn"

options :: [Bool]
options = [True , False]
