module HelVM.HelMA.Common.Collections.SplitAt where

import Prelude  hiding (splitAt)

import qualified Data.Sequence as Seq
import qualified Prelude       as List (splitAt)

class SplitAt e c | c -> e where
  splitAt  :: Int -> c -> (c , c)

instance SplitAt e [e] where
  splitAt i c = List.splitAt i c

instance SplitAt e (Seq e) where
  splitAt i c = Seq.splitAt i c
