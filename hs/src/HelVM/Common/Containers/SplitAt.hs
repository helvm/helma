module HelVM.Common.Containers.SplitAt where

import Prelude  hiding (break , drop , span , splitAt)

import qualified Data.Sequence as Seq
import qualified Prelude       as List (break , drop , span , splitAt)

splitBy :: (Eq e , SplitAt e c) => e -> c -> (c , c)
splitBy separator c = (acc , drop 1 c') where (acc , c') = break (== separator) c

class SplitAt e c | c -> e where
  drop     :: Int -> c -> c
  splitAt  :: Int -> c -> (c , c)
  span     :: (e -> Bool) -> c -> (c , c)
  break    :: (e -> Bool) -> c -> (c , c)

instance SplitAt e [e] where
  drop    = List.drop
  splitAt = List.splitAt
  span    = List.span
  break   = List.break

instance SplitAt e (Seq e) where
  drop    = Seq.drop
  splitAt = Seq.splitAt
  span    = Seq.spanl
  break   = Seq.breakl
