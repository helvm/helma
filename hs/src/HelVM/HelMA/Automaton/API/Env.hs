module HelVM.HelMA.Automaton.API.Env where

import           HelVM.HelMA.Automaton.API.IOTypes

import           RIO

type Has env = (RIO.HasLogFunc env, HasFileIO env)

readTextFileRio :: Has env => FilePath -> RIO.RIO env Source
readTextFileRio = (RIO.view fileIOL >>=) . flip readTextFile

data Env = Env
  { envLogFunc :: LogFunc
  , envFileIO  :: FileIO
  }

newtype FileIO = FileIO
  { readTextFile :: forall env. (HasLogFunc env) => FilePath -> RIO env Source
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

class HasFileIO env where
  fileIOL :: Lens' env FileIO

instance HasFileIO Env where
  fileIOL = lens envFileIO (\x y -> x { envFileIO = y })
