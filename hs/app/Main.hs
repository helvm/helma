module Main where

import           Options

import           HelVM.HelMA.Evaluator

import           HelVM.HelIO.Extra                    (readFileTextUtf8)
import qualified HelVM.HelMA.Automaton.API.AppOptions as App
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.LogLevel

import           Options.Applicative

import qualified Codec.Picture                        as Picture

import qualified Data.ByteString.Lazy                 as LByteString
import qualified Data.Text.Lazy.IO                    as LText

import           Prelude

import qualified RIO

import           System.Environment                   (getProgName)
import qualified System.IO                            as IO

main ∷ IO ()
main = do
  progName <- getProgName
  opts     <- execParser (optsInfo progName)
  hSetBuffering stdout IO.NoBuffering
  logOptions <- RIO.logOptionsHandle stderr True
  RIO.withLogFunc (setLogMinLevel opts logOptions) (runApp opts)
  exitSuccess

setLogMinLevel ∷ App.AppOptions → RIO.LogOptions → RIO.LogOptions
setLogMinLevel = RIO.setLogMinLevel . toRio . App.verbosity

optsInfo ∷ String → ParserInfo App.AppOptions
optsInfo progName = info (optionsParser <**> helper <**> versionInfo progName)
  (  fullDesc
  <> progDesc "Runs esoteric programs - complete with pretty bad error messages"
  <> header (progName <> ": The Interpreter of BrainFuck , ETA , LazyK , Piet , SubLeq , WhiteSpace , Zot")
  )

versionInfo ∷ String → Parser (a → a)
versionInfo _ = infoOption "1.0.0"
  (  long "version"
  <> help "print version information and exit")

runApp ∷ MonadIO m ⇒ App.AppOptions → RIO.LogFunc →  m ()
runApp = (liftIO .) . fmap (`RIO.runRIO` runRio) . productionEnv

productionEnv ∷ App.AppOptions → RIO.LogFunc → Env
productionEnv = Env productionFileIO defaultStdIO

productionFileIO ∷ FileIO
productionFileIO = FileIO
  { readTextFile = readFileTextUtf8
  , readImage = readDynamicImage
  }

defaultStdIO ∷ StdIO
defaultStdIO = StdIO
  { stdPutLTextLn      = putLTextLn
  , stdGetContentsText = liftIO LText.getContents
  , stdPutLBSLn        = putLBSLn
  , stdGetContentsBS   = liftIO LByteString.getContents
  }

readDynamicImage ∷ MonadIO m ⇒ FilePath → m Picture.DynamicImage
readDynamicImage = liftIO . (Picture.readImage >=> either RIO.throwString pure)
