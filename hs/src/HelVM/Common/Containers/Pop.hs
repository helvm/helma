module HelVM.Common.Containers.Pop where

import HelVM.Common.Safe

import Data.Sequence (Seq(..))

discard :: (MonadSafeError m , Pop1 e c) => c -> m c
discard s = discard' <$> pop1 s where
  discard' (_ , s') = s'

class Pop1 e c | c -> e where
  pop1 :: MonadSafeError m => c -> m (e , c)

instance Show e => Pop1 e [e] where
  pop1 (e  : c) = pure (e , c)
  pop1       c  = liftErrorTuple ("Empty" , show c)

instance Show e => Pop1 e (Seq e) where
  pop1 (e :<|  c) = pure (e , c)
  pop1         c  = liftErrorTuple ("Empty" , show c)

class Pop2 e c | c -> e where
  pop2 :: c -> MonadSafeError m => m (e , e , c)

instance Show e => Pop2 e [e] where
  pop2 (e : e' : c) = pure (e , e', c)
  pop2           c  = liftErrorTuple ("Empty" , show c)

instance Show e => Pop2 e (Seq e) where
  pop2 (e :<| e' :<| c) = pure (e , e', c)
  pop2               c  = liftErrorTuple ("Empty " , show c)
