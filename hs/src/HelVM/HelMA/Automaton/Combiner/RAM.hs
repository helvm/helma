module HelVM.HelMA.Automaton.Combiner.RAM
  ( RAM
  , flippedStoreChar
  , fromList
  , genericLoad
  , genericStore
  , load
  , store
  , storeChar
  ) where

import           HelVM.HelIO.Containers.MTIndexSafe
import           HelVM.HelIO.Containers.MTInsertDef

import           Data.Default
import           Data.MonoTraversable
import           Data.Sequences
import           Prelude                            hiding ( divMod, drop, fromList, splitAt, swap )

genericLoad ∷ (Integral i , RAM ll element) ⇒ ll → i → element
genericLoad l = load l . fromIntegral

load ∷ (RAM ll element) ⇒ ll → Address → element
load l i = indexMaybe l i ?: def

flippedStoreChar ∷ (Num element , Integral address , RAM ll element) ⇒ address → ll → Char → ll
flippedStoreChar a = flip (storeChar a)

storeChar ∷ (Num element , Integral address , RAM ll element) ⇒ address → Char → ll → ll
storeChar a char = genericStore a $ ord char

genericStore ∷ (Integral value , Num element , Integral address , RAM ll element) ⇒ address → value → ll → ll
genericStore a v = store a $ fromIntegral v

store ∷ (Integral a , RAM ll element) ⇒ a → element → ll → ll
store = insertDef . fromIntegral

-- | Types
type RAM ll element = (Default element , Element ll ~ element , Index ll ~ Int , LL ll)

type LL ll = (Show ll , IsSequence ll , InsertDef ll , IndexSafe ll)

type Address = Int
