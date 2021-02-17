{-# Language GeneralizedNewtypeDeriving #-}
module HelVM.HelCam.Common.RAM.ListRAM (
  RAM,
  empty,
  fromList,
  load,
  store
) where

import HelVM.HelCam.Common.Util

import Data.Default

import Prelude hiding (empty, fromList)

newtype RAM s = MakeRAM [s] deriving (Foldable)
type DRAM s = D (RAM s)

-- Constructors
empty :: Default s => RAM s
empty = MakeRAM []

fromList :: Default s => [s] -> RAM s
fromList = MakeRAM

-- Mutators
load :: (Integral a, Default s) => RAM s -> a -> s
load (MakeRAM m) address = index' m (fromIntegral address) ?: def

store :: (Integral a, Default s) => a -> s -> DRAM s
store address symbol (MakeRAM m) = MakeRAM $ insert' (fromIntegral address) symbol m

-- Private
index' :: [s] -> Int -> Maybe s
index' = (!!?)

insert' :: Default s => Int -> s -> [s] -> [s]
insert' 0       symbol []     = [symbol]
insert' 0       symbol (_:xs) = symbol : xs
insert' address symbol []     = def    : insert' (address-1) symbol []
insert' address symbol (x:xs) = x      : insert' (address-1) symbol xs
