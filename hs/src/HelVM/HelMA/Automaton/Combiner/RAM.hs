{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automaton.Combiner.RAM (
  genericLoad,
  load,
  flippedStoreChar,
  storeChar,
  genericStore,
  store,
  RAM,
) where

import           HelVM.HelIO.Containers.MTIndexSafe
import           HelVM.HelIO.Containers.MTInsertDef

import           Data.Default
import           Data.MonoTraversable               (Element)
import           Data.Sequences                     (Index)
import           Prelude                            hiding (divMod, drop, splitAt, swap)

genericLoad :: (Integral i , RAM ll) => ll -> i -> Element ll
genericLoad l = load l . fromIntegral

load :: RAM ll => ll -> Address -> Element ll
load l i = indexMaybe l (fromIntegral i) ?: def

flippedStoreChar :: (Integral address , RAM ll , Num (Element ll)) => address -> ll -> Char -> ll
flippedStoreChar a = flip (storeChar a)

storeChar :: (Integral address , RAM ll , Num (Element ll)) => address -> Char -> ll -> ll
storeChar a char = genericStore a $ ord char

genericStore :: (Integral value , Integral address , RAM ll , Num (Element ll)) => address -> value -> ll -> ll
genericStore a v = store a $ fromIntegral v

store :: (Integral a , RAM ll) => a -> Element ll -> ll -> ll
store = insertDef . fromIntegral

-- | Types
type RAM ll = (Show ll , Default (Element ll) , II ll)

type II ll = (InsertDef ll , IndexSafe ll , Integral (Index ll))

type Address = Int
