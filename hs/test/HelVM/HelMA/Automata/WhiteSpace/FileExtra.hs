module HelVM.HelMA.Automata.WhiteSpace.FileExtra (
  readFileByTokenType,
  readWsFile,
  readStnFile,
  readExtFile,
  buildAbsoluteTokenFileName,
  buildAbsoluteWsFileName,
  buildAbsoluteStnFileName,
  buildAbsoluteWsIlFileName,
  buildAbsoluteWsOutFileName,
  buildAbsoluteWsLogFileName,
  tokenTypeToExt,
  options,
) where

import           HelVM.HelMA.Automata.FileExtra

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Types.TokenType

readFileByTokenType :: TokenType -> FilePath -> IO Source
readFileByTokenType tokenType = readSourceFile . buildAbsoluteTokenFileName tokenType

readWsFile :: FilePath -> IO Source
readWsFile = readSourceFile . buildAbsoluteWsFileName

readStnFile :: FilePath -> IO Source
readStnFile = readSourceFile . buildAbsoluteStnFileName

readExtFile :: FilePath -> FilePath -> IO Source
readExtFile ext = readSourceFile . buildAbsoluteExtFileName ext lang

buildAbsoluteTokenFileName :: TokenType -> FilePath -> FilePath
buildAbsoluteTokenFileName = flip buildAbsoluteExtFileName lang . tokenTypeToExt

buildAbsoluteWsFileName :: FilePath -> FilePath
buildAbsoluteWsFileName = buildAbsoluteExtFileName lang lang

buildAbsoluteStnFileName :: FilePath -> FilePath
buildAbsoluteStnFileName = buildAbsoluteExtFileName stn lang

buildAbsoluteWsIlFileName :: FilePath -> FilePath
buildAbsoluteWsIlFileName = buildAbsoluteIlFileName lang

buildAbsoluteWsOutFileName :: FilePath -> FilePath
buildAbsoluteWsOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteWsLogFileName :: FilePath -> FilePath
buildAbsoluteWsLogFileName = buildAbsoluteLogFileName lang

tokenTypeToExt :: TokenType -> FilePath
tokenTypeToExt WhiteTokenType   = lang
tokenTypeToExt VisibleTokenType = stn

stn :: FilePath
stn = "stn"

lang :: FilePath
lang = "ws"
