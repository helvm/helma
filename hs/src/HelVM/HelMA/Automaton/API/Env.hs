module HelVM.HelMA.Automaton.API.Env where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import qualified Codec.Picture                        as Picture

import qualified RIO

type Has env = (HasIO env, HasAppOptions env, RIO.HasLogFunc env)
type HasIO env = (HasFileIO env, HasStdIO env)

readSourceFileRio ∷ Has env ⇒ RIO.RIO env Source
readSourceFileRio = readSourceFileWithOptions =<< optionsRio where
  readSourceFileWithOptions = readSourceFile <$> exec <*> file
  readSourceFile True = pure . toText
  readSourceFile _    = readTextFileRio

readTextFileRio ∷ Has env ⇒ FilePath → RIO.RIO env Source
readTextFileRio = (RIO.view fileIOL >>=) . flip readTextFile

readImageRio ∷ Has env ⇒ FilePath → RIO.RIO env Picture.DynamicImage
readImageRio = (RIO.view fileIOL >>=) . flip readImage

putLTextLnRio ∷ Has env ⇒ LText → RIO.RIO env ()
putLTextLnRio = (RIO.view stdIOL >>=) . flip stdPutLTextLn

getContentsTextRio ∷ Has env ⇒ RIO.RIO env LText
getContentsTextRio = RIO.view stdIOL >>= stdGetContentsText

putLBSLnRio ∷ Has env ⇒ LByteString → RIO.RIO env ()
putLBSLnRio = (RIO.view stdIOL >>=) . flip stdPutLBSLn

getContentsBSRio ∷ Has env ⇒ RIO.RIO env LByteString
getContentsBSRio = RIO.view stdIOL >>= stdGetContentsBS

optionsRio ∷ Has env ⇒ RIO.RIO env AppOptions
optionsRio = RIO.view appOptionsL

logFuncRio ∷ Has env ⇒ RIO.RIO env RIO.LogFunc
logFuncRio = RIO.view RIO.logFuncL

data Env
  = Env
      { envFileIO  :: FileIO
      , envStdIO   :: StdIO
      , envOptions :: AppOptions
      , envLogFunc :: RIO.LogFunc
      }

data FileIO
  = FileIO
      { readTextFile :: forall env. FilePath -> RIO.RIO env Source
      , readImage    :: forall env. FilePath -> RIO.RIO env Picture.DynamicImage
      }

class HasFileIO env where
  fileIOL :: RIO.Lens' env FileIO

instance HasFileIO Env where
  fileIOL = RIO.lens envFileIO (\x y -> x { envFileIO = y })

data StdIO
  = StdIO
      { stdPutLTextLn      :: forall env. LText -> RIO.RIO env ()
      , stdGetContentsText :: forall env. RIO.RIO env LText
      , stdPutLBSLn        :: forall env. LByteString -> RIO.RIO env ()
      , stdGetContentsBS   :: forall env. RIO.RIO env LByteString
      }

class HasStdIO env where
  stdIOL :: RIO.Lens' env StdIO

instance HasStdIO Env where
  stdIOL = RIO.lens envStdIO (\x y -> x { envStdIO = y })

class HasAppOptions env where
  appOptionsL :: RIO.Lens' env AppOptions

instance HasAppOptions Env where
  appOptionsL = RIO.lens envOptions (\x y -> x { envOptions = y })

instance RIO.HasLogFunc Env where
  logFuncL = RIO.lens envLogFunc (\x y -> x { envLogFunc = y })
