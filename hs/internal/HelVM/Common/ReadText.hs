module HelVM.Common.ReadText (
  readTextUnsafe,
  readTextSafe,
  readTextMaybe,
  readUnsafe,
  readSafe,
) where

import           HelVM.Common.Control.Safe

readTextUnsafe :: Read a => Text -> a
readTextUnsafe = unsafe . readTextSafe

readTextSafe :: (MonadSafe m , Read a) => Text -> m a
readTextSafe a = (appendError a . liftEitherError . readEither . toString) a

readTextMaybe :: Read a => Text -> Maybe a
readTextMaybe = readMaybe . toString

readUnsafe :: Read a => String -> a
readUnsafe = unsafe . readSafe

readSafe :: (MonadSafe m , Read a) => String -> m a
readSafe a = (appendError (toText a) . liftEitherError . readEither) a
