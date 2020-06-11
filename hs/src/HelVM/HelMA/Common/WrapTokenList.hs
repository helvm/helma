module HelVM.HelMA.Common.WrapTokenList where

import qualified Text.Read as Read
import qualified Text.Show

newtype WrapTokenList a = WrapTokenList { unWrapTokenList :: a }
  deriving (Eq)

----

instance Show a => Show (WrapTokenList [a]) where
  show (WrapTokenList tokens) = show =<< tokens

instance Read a => Read (WrapTokenList [a]) where
  readsPrec _ source = [( WrapTokenList $ maybeToList . readMaybe . one =<< source, "")]
