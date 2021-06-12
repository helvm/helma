module HelVM.HelMA.Automaton.WrapTokenList where

import qualified Text.Read
import qualified Text.Show

newtype WrapTokenList a = WrapTokenList { unWrapTokenList :: a }
  deriving stock (Eq)

----

instance Show a => Show (WrapTokenList [a]) where
  show (WrapTokenList tokens) = show =<< tokens

instance Read a => Read (WrapTokenList [a]) where
  readsPrec _ source = [( WrapTokenList $ maybeToList . readMaybe . one =<< source , "")]
