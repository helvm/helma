module Env where



import           RIO

newtype Env = Env { envLogFunc :: LogFunc }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
