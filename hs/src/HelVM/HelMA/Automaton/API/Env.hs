{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module HelVM.HelMA.Automaton.API.Env
  ( Env (..)
  , FileIO (..)
  , Has
  , HasAppOptions (..)
  , HasFileIO (..)
  , HasIO
  , HasStdIO (..)
  , StdIO (..)
  , envFileIOL
  , envLogFuncL
  , envOptionsL
  , envStdIOL
  , getContentsBSRio
  , getContentsTextRio
  , logFuncRio
  , optionsRio
  , putLBSLnRio
  , putLTextLnRio
  , readImageRio
  , readSourceFileRio
  , readTextFileRio
  ) where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import qualified Codec.Picture                        as Picture

import qualified RIO

-- TYPES & STRUCTS

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
      { envFileIO  :: FileIO
      , envStdIO   :: StdIO
      , envOptions :: AppOptions
      , envLogFunc :: RIO.LogFunc
      }

envFileIOL ∷ RIO.Lens' Env FileIO
envFileIOL = RIO.lens envFileIO (\s x -> s { envFileIO = x })

envStdIOL ∷ RIO.Lens' Env StdIO
envStdIOL = RIO.lens envStdIO (\s x -> s { envStdIO = x })

envOptionsL ∷ RIO.Lens' Env AppOptions
envOptionsL = RIO.lens envOptions (\s x -> s { envOptions = x })

envLogFuncL ∷ RIO.Lens' Env RIO.LogFunc
envLogFuncL = RIO.lens envLogFunc (\s x -> s { envLogFunc = x })

-- HAS CLASSES & INSTANCES

type Has env = (HasIO env, HasAppOptions env, RIO.HasLogFunc env)
type HasIO env = (HasFileIO env, HasStdIO env)

class HasFileIO env where
  fileIOL :: RIO.Lens' env FileIO

instance HasFileIO Env where
  fileIOL = envFileIOL

class HasStdIO env where
  stdIOL :: RIO.Lens' env StdIO

instance HasStdIO Env where
  stdIOL = envStdIOL

class HasAppOptions env where
  appOptionsL :: RIO.Lens' env AppOptions

instance HasAppOptions Env where
  appOptionsL = envOptionsL

instance RIO.HasLogFunc Env where
  logFuncL = envLogFuncL

-- RIO HELPERS

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
