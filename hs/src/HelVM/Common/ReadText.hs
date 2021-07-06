module HelVM.Common.ReadText (
  readText,
  readTextSafe,
  readTextMaybe,
) where

import HelVM.Common.Safe

readText :: Read a => Text -> a
readText = unsafe . readTextSafe where
  unsafe (Right a) = a
  unsafe (Left a) = error $ errorsToText a

readTextSafe :: (MonadSafeError m , Read a) => Text -> m a
readTextSafe a = appendError a $ liftEitherError $ readEither $ toString a

readTextMaybe :: Read a => Text -> Maybe a
readTextMaybe = readMaybe . toString
