module HelVM.Common.Containers.Pop where

import HelVM.Common.Safe

import Data.Sequence (Seq(..))

discard :: Pop1 e c => c -> Safe c
discard s = discard' <$> pop1 s where
  discard' (_ , s') = s'

class Pop1 e c | c -> e where
  pop1 :: c -> Safe (e , c)

instance Show e => Pop1 e [e] where
  pop1 (e  : c) = safe (e , c)
  pop1       c  = safeErrorTuple ("Empty" , show c)

instance Show e => Pop1 e (Seq e) where
  pop1 (e :<|  c) = safe (e , c)
  pop1         c  = safeErrorTuple ("Empty" , show c)

class Pop2 e c | c -> e where
  pop2 :: c -> Safe (e , e , c)

instance Show e => Pop2 e [e] where
  pop2 (e : e' : c) = safe (e , e', c)
  pop2           c  = safeErrorTuple ("Empty" , show c)

instance Show e => Pop2 e (Seq e) where
  pop2 (e :<| e' :<| c) = safe (e , e', c)
  pop2               c  = safeErrorTuple ("Empty " , show c)
