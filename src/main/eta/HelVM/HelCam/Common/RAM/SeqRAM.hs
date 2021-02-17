{-# Language GeneralizedNewtypeDeriving #-}
module HelVM.HelCam.Common.RAM.SeqRAM (
  RAM,
  HelVM.HelCam.Common.RAM.SeqRAM.empty,
  HelVM.HelCam.Common.RAM.SeqRAM.fromList,
  load,
  store
) where

import HelVM.HelCam.Common.Util

import Data.Default
import Data.Sequence as Seq

newtype RAM s = MakeRAM (Seq s) deriving (Foldable)
type DRAM s = D (RAM s)

-- Constructors
empty :: Default s => RAM s
empty = MakeRAM Seq.empty

fromList :: Default s => [s] -> RAM s
fromList = MakeRAM . Seq.fromList

-- Mutators
load :: (Integral a, Default s) => RAM s -> a -> s
load (MakeRAM m) address = index' m (fromIntegral address) ?: def

store :: (Integral a, Default s) => a -> s -> DRAM s
store address symbol (MakeRAM m) = MakeRAM $ insert' (fromIntegral address) symbol m

-- Private
index' :: Seq s -> Int -> Maybe s
index' = (!?)

insert' :: Default s => Int -> s -> Seq s -> Seq s
insert' address symbol m = insert'' (Seq.length m) where
  insert'' l
    | address < l  = Seq.update address symbol m
    | otherwise    = m <> Seq.replicate (address - l) def |> symbol
