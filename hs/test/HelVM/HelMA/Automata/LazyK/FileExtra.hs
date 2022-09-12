module HelVM.HelMA.Automata.LazyK.FileExtra (
  readLazyKFile,
  buildAbsoluteMinifiedLazyKFileName,
  buildAbsoluteLazyKFileName,
  buildAbsoluteLazyKLambdaFileName,
  buildAbsoluteLazyKOutFileName,
  buildAbsoluteLazyKLogFileName,
  showCompile,
  options,
) where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes

readLazyKFile :: FilePath -> IO Source
readLazyKFile = readSourceFile . buildAbsoluteLazyKFileName

buildAbsoluteMinifiedLazyKFileName :: FilePath -> FilePath
buildAbsoluteMinifiedLazyKFileName = buildAbsoluteModeFileName "minified" lang

buildAbsoluteLazyKFileName :: FilePath -> FilePath
buildAbsoluteLazyKFileName = buildAbsoluteLangFileName lang

buildAbsoluteLazyKLambdaFileName :: FilePath -> FilePath
buildAbsoluteLazyKLambdaFileName = buildAbsoluteLambdaFileName lang

buildAbsoluteLazyKOutFileName :: FilePath -> FilePath
buildAbsoluteLazyKOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteLazyKLogFileName :: FilePath -> FilePath
buildAbsoluteLazyKLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "lazy"

showCompile :: Bool -> FilePath
showCompile False = "token"
showCompile True  = "instruction"
