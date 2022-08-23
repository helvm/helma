module HelVM.Common.Containers.MTInsertDef where

import           Control.Type.Operator

import           Data.Default
import           Data.MonoTraversable
import           Data.Sequence         ((|>))
import           Data.Sequences

import qualified Data.IntMap           as IntMap
import qualified Data.Sequence         as Seq

-- | Insert a new element
naturalInsertDef :: (InsertDef seq , Num $ Index seq) => Natural -> Element seq -> seq -> seq
naturalInsertDef = insertDef . fromIntegral

-- | Type Class
class InsertDef seq where
  insertDef :: Index seq -> Element seq -> seq -> seq

instance Default a => InsertDef [a] where
  insertDef 0 e []     = [e]
  insertDef 0 e (_:xs) = e   : xs
  insertDef i e []     = def : insertDef (i-1) e []
  insertDef i e (x:xs) = x   : insertDef (i-1) e xs

instance Default a => InsertDef (Seq a) where
  insertDef i e c = (check . Seq.length) c where
    check l
      | i < l       = Seq.update i e c
      | otherwise   = c <> Seq.replicate (i - l) def |> e

instance Index (IntMap a) ~ Int => InsertDef (IntMap a) where
  insertDef = IntMap.insert
