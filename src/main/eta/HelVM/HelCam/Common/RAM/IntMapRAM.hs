{-# Language GeneralizedNewtypeDeriving #-}
module HelVM.HelCam.Common.RAM.IntMapRAM (
  HelVM.HelCam.Common.RAM.IntMapRAM.RAM,
  HelVM.HelCam.Common.RAM.IntMapRAM.empty,
  HelVM.HelCam.Common.RAM.IntMapRAM.fromList,
  load,
  store
) where

import HelVM.HelCam.Common.Util

import Data.Default
import Data.IntMap as IntMap

newtype RAM s = MakeRAM (IntMap s) deriving (Foldable)
type DRAM s = D (RAM s)

-- Constructors
empty :: Default s => RAM s
empty = MakeRAM IntMap.empty

fromList :: Default s => [s] -> RAM s
fromList list = MakeRAM $ IntMap.fromList $ zip [0..] list

-- Mutators
load :: (Integral a, Default s) => RAM s -> a -> s
load (MakeRAM m) address = index' m (fromIntegral address) ?: def

store :: (Integral a, Default s) => a -> s -> DRAM s
store address symbol (MakeRAM m) = MakeRAM $ insert' (fromIntegral address) symbol m

-- Private
index' :: IntMap s -> Int -> Maybe s
index' = (!?)

insert' :: Int -> s -> IntMap s -> IntMap s
insert' = insert
