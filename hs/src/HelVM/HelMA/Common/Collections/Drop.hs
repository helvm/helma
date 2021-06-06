module HelVM.HelMA.Common.Collections.Drop where

import Prelude  hiding (drop)

import qualified Data.Sequence as Seq
import qualified Prelude       as List (drop)

class Drop e c | c -> e where
  drop :: Int -> c -> c

instance Drop e [e] where
  drop i c = List.drop i c

instance Drop e (Seq e) where
  drop i c = Seq.drop i c
