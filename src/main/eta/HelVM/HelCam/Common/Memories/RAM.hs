{-# Language AllowAmbiguousTypes   #-}
{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}
module HelVM.HelCam.Common.Memories.RAM (
  RAM,
  HelVM.HelCam.Common.Memories.RAM.empty,
  HelVM.HelCam.Common.Memories.RAM.fromList,
  load,
  store
) where

import Data.Default
import Data.IntMap as IntMap
import Data.Sequence as Seq

type Address = Int

load :: (Integral a, Default s, RAM s m) => m -> a -> s
load memory address = index' memory (fromIntegral address) ?: def

store :: (Integral a, Default s, RAM s m) => a -> s -> m -> m
store address = insert' (fromIntegral address)

class Default s => RAM s m where
  fromList :: [s] -> m
  empty    :: m
  index'   :: m -> Address -> Maybe s
  insert'  :: Address -> s -> m -> m

instance Default s => RAM s [s] where
  fromList = id
  empty    = []
  index'   = (!!?)
  insert' 0       symbol []     = [symbol]
  insert' 0       symbol (_:xs) = symbol : xs
  insert' address symbol []     = def    : insert' (address-1) symbol []
  insert' address symbol (x:xs) = x      : insert' (address-1) symbol xs

instance Default s => RAM s (Seq s) where
  fromList = Seq.fromList
  empty    = Seq.fromList []
  index'   = (Seq.!?)
  insert' address symbol memory = insert'' (Seq.length memory) where
    insert'' l
      | address < l = Seq.update address symbol memory
      | otherwise   = memory <> Seq.replicate (address - l) def |> symbol

instance Default s => RAM s (IntMap s) where
  fromList list = IntMap.fromList $ Prelude.zip [0..] list
  empty         = IntMap.empty
  index'        = (IntMap.!?)
  insert'       = IntMap.insert
