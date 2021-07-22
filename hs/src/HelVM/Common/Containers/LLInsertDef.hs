module HelVM.Common.Containers.LLInsertDef where

import           Data.Default
import           Data.Sequence ((|>))

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

-- | Insert a new element
naturalInsertDef :: InsertDef full item => Natural -> item -> full -> full
naturalInsertDef = insertDef . fromIntegral

-- | Type Class
class InsertDef full item | full -> item where
  insertDef :: Int -> item -> full -> full

instance Default a => InsertDef [a] a where
  insertDef 0 e []     = [e]
  insertDef 0 e (_:xs) = e   : xs
  insertDef i e []     = def : insertDef (i-1) e []
  insertDef i e (x:xs) = x   : insertDef (i-1) e xs

instance Default a => InsertDef (Seq a) a where
  insertDef i e c = (check . Seq.length) c where
    check l
      | i < l       = Seq.update i e c
      | otherwise   = c <> Seq.replicate (i - l) def |> e

instance InsertDef (IntMap a) a where
  insertDef = IntMap.insert
