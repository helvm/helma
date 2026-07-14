module HelVM.HelMA.Automaton.API.Env where

import           HelVM.HelMA.Automaton.API.AppOptions
import           HelVM.HelMA.Automaton.API.IOTypes

import           RIO

type Has env = (RIO.HasLogFunc env, HasFileIO env, HasAppOptions env)

readSourceFileRio :: Has env => RIO.RIO env Source
readSourceFileRio = readSourceFileWithOptions =<< optionsRio where
  readSourceFileWithOptions = readSourceFile <$> exec <*> file
  readSourceFile True = pure . toText
  readSourceFile _    = readTextFileRio

readTextFileRio :: Has env => FilePath -> RIO.RIO env Source
readTextFileRio = (RIO.view fileIOL >>=) . flip readTextFile

optionsRio :: Has env => RIO.RIO env AppOptions
optionsRio = RIO.view appOptionsL

data Env = Env
  { envFileIO  :: FileIO
  , envOptions :: AppOptions
  , envLogFunc :: LogFunc
  }

newtype FileIO = FileIO
  { readTextFile :: forall env. (HasLogFunc env) => FilePath -> RIO env Source
  }

class HasFileIO env where
  fileIOL :: Lens' env FileIO

instance HasFileIO Env where
  fileIOL = lens envFileIO (\x y -> x { envFileIO = y })

class HasAppOptions env where
  appOptionsL :: Lens' env AppOptions

instance HasAppOptions Env where
  appOptionsL = lens envOptions (\x y -> x { envOptions = y })

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

