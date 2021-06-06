module HelVM.HelMA.Common.Collections.Pop where

import Data.Sequence (Seq(..))

class Pop1 e c | c -> e where
  pop1 :: c -> (e , c)

instance Show e => Pop1 e [e] where
  pop1 (e  : c) = (e , c)
  pop1       c  = error $ "Empty " <> show c

instance Show e => Pop1 e (Seq e) where
  pop1 (e :<|  c) = (e , c)
  pop1         c  = error $ "Empty " <> show c

class Pop2 e c | c -> e where
  pop2 :: c -> (e , e , c)

instance Show e => Pop2 e [e] where
  pop2 (e : e' : c) = (e , e', c)
  pop2           c  = error $ "Empty " <> show c

instance Show e => Pop2 e (Seq e) where
  pop2 (e :<| e' :<| c) = (e , e', c)
  pop2               c  = error $ "Empty  " <> show c

