module HelVM.Common.Containers.Reverse where

import Prelude  hiding (reverse)

import qualified Data.Sequence as Seq
import qualified Prelude       as List (reverse)

class Reverse e c | c -> e where
  reverse :: c -> c

instance Reverse e [e] where
  reverse = List.reverse

instance Reverse e (Seq e) where
  reverse = Seq.reverse
