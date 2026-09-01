{-# LANGUAGE FlexibleInstances #-}
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
      { readTextFile :: FilePath -> IO Source
      , readImage    :: FilePath -> IO Picture.DynamicImage
      }

data StdIO
  = StdIO
      { stdPutLTextLn      :: LText -> IO ()
      , stdGetContentsText :: IO LText
      , stdPutLBSLn        :: LByteString -> IO ()
      , stdGetContentsBS   :: IO LByteString
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

readSourceFile ∷ Has env ⇒ Bool → FilePath → RIO.RIO env Text
readSourceFile True = pure . toText
readSourceFile _    = readTextFileRio

readTextFileRio ∷ Has env ⇒ FilePath → RIO.RIO env Source
readTextFileRio fp = RIO.liftIO . (`readTextFile` fp) =<< RIO.view fileIOL

readImageRio ∷ Has env ⇒ FilePath → RIO.RIO env Picture.DynamicImage
readImageRio fp = RIO.liftIO . (`readImage` fp) =<< RIO.view fileIOL

putLTextLnRio ∷ Has env ⇒ LText → RIO.RIO env ()
putLTextLnRio text = RIO.liftIO . (`stdPutLTextLn` text) =<< RIO.view stdIOL

getContentsTextRio ∷ Has env ⇒ RIO.RIO env LText
getContentsTextRio = RIO.liftIO . stdGetContentsText =<< RIO.view stdIOL

putLBSLnRio ∷ Has env ⇒ LByteString → RIO.RIO env ()
putLBSLnRio lbs = RIO.liftIO . (`stdPutLBSLn` lbs) =<< RIO.view stdIOL

getContentsBSRio ∷ Has env ⇒ RIO.RIO env LByteString
getContentsBSRio = RIO.liftIO . stdGetContentsBS =<< RIO.view stdIOL

optionsRio ∷ Has env ⇒ RIO.RIO env AppOptions
optionsRio = RIO.view appOptionsL

logFuncRio ∷ Has env ⇒ RIO.RIO env RIO.LogFunc
logFuncRio = RIO.view RIO.logFuncL
