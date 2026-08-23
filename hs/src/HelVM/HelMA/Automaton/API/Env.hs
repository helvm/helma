{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module HelVM.HelMA.Automaton.API.Env where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import qualified Codec.Picture                        as Picture

import           Lens.Micro.Platform

import qualified RIO

data FileIO
  = FileIO
      { readTextFile :: forall env. FilePath -> RIO.RIO env Source
      , readImage    :: forall env. FilePath -> RIO.RIO env Picture.DynamicImage
      }

data StdIO
  = StdIO
      { stdPutLTextLn      :: forall env. LText -> RIO.RIO env ()
      , stdGetContentsText :: forall env. RIO.RIO env LText
      , stdPutLBSLn        :: forall env. LByteString -> RIO.RIO env ()
      , stdGetContentsBS   :: forall env. RIO.RIO env LByteString
      }

data Env
  = Env
      { _envFileIO  :: FileIO
      , _envStdIO   :: StdIO
      , _envOptions :: AppOptions
      , _envLogFunc :: RIO.LogFunc
      }

makeLenses ''Env

type Has env = (HasIO env, HasAppOptions env, RIO.HasLogFunc env)
type HasIO env = (HasFileIO env, HasStdIO env)

class HasFileIO env where
  fileIOL :: RIO.Lens' env FileIO

instance HasFileIO Env where
  fileIOL = envFileIO

class HasStdIO env where
  stdIOL :: RIO.Lens' env StdIO

instance HasStdIO Env where
  stdIOL = envStdIO

class HasAppOptions env where
  appOptionsL :: RIO.Lens' env AppOptions

instance HasAppOptions Env where
  appOptionsL = envOptions

instance RIO.HasLogFunc Env where
  logFuncL = envLogFunc

readSourceFileRio ∷ Has env ⇒ RIO.RIO env Source
readSourceFileRio = readSourceFileWithOptions =<< optionsRio where
  readSourceFileWithOptions = readSourceFile <$> exec <*> file
  readSourceFile True = pure . toText
  readSourceFile _    = readTextFileRio

readTextFileRio ∷ Has env ⇒ FilePath → RIO.RIO env Source
readTextFileRio fp = do
  io <- RIO.view fileIOL
  readTextFile io fp

readImageRio ∷ Has env ⇒ FilePath → RIO.RIO env Picture.DynamicImage
readImageRio fp = do
  io <- RIO.view fileIOL
  readImage io fp

putLTextLnRio ∷ Has env ⇒ LText → RIO.RIO env ()
putLTextLnRio text = do
  io <- RIO.view stdIOL
  stdPutLTextLn io text

getContentsTextRio ∷ Has env ⇒ RIO.RIO env LText
getContentsTextRio = do
  io <- RIO.view stdIOL
  stdGetContentsText io

putLBSLnRio ∷ Has env ⇒ LByteString → RIO.RIO env ()
putLBSLnRio lbs = do
  io <- RIO.view stdIOL
  stdPutLBSLn io lbs

getContentsBSRio ∷ Has env ⇒ RIO.RIO env LByteString
getContentsBSRio = do
  io <- RIO.view stdIOL
  stdGetContentsBS io

optionsRio ∷ Has env ⇒ RIO.RIO env AppOptions
optionsRio = RIO.view appOptionsL

logFuncRio ∷ Has env ⇒ RIO.RIO env RIO.LogFunc
logFuncRio = RIO.view RIO.logFuncL
