module HelVM.Common.Safe (
  safeFailToFail,
  safeToFail,
  safeLegacyToFail,

  safe,
  safeLegacyToSafe,
  safeToSafeLegacy,

  maybeToSafeOrErrorTupleList,
  maybeToSafeOrErrorTuple,
  maybeToSafeOrError,

  safeErrorTupleList,
  safeErrorTuple,
  safeError,

  appendErrorTupleList,
  appendErrorTuple,
  appendError,

  tupleListToError,
  tupleToError,

  unsafe,

  SafeFail_,
  SafeFail,
  Safe_,
  Safe,
  Error,
) where

safeFailToFail ::  MonadFail m => SafeFail m a -> m a
safeFailToFail m = safeToFail =<< m

safeToFail ::  MonadFail m => Safe a -> m a
safeToFail = safeLegacyToFail . safeToSafeLegacy

safeLegacyToFail :: MonadFail m => SafeLegacy a -> m a
safeLegacyToFail (Right a) = pure a
safeLegacyToFail (Left a)  = fail a

-- Create Safe

safe :: a -> Safe a
safe = pure

safeLegacyToSafe :: SafeLegacy a -> Safe a
safeLegacyToSafe = first toText

safeToSafeLegacy :: Safe a -> SafeLegacy a
safeToSafeLegacy = first toString

---- Create from Maybe

maybeToSafeOrErrorTupleList :: [ErrorTuple] -> Maybe a -> Safe a
maybeToSafeOrErrorTupleList = maybeToSafeOrError . tupleListToError

maybeToSafeOrErrorTuple :: ErrorTuple -> Maybe a -> Safe a
maybeToSafeOrErrorTuple = maybeToSafeOrError . tupleToError

maybeToSafeOrError :: Error -> Maybe a -> Safe a
maybeToSafeOrError = maybeToRight

---- Create Error

safeErrorTupleList :: [ErrorTuple] -> Safe a
safeErrorTupleList = safeError . tupleListToError

safeErrorTuple :: ErrorTuple -> Safe a
safeErrorTuple = safeError . tupleToError

safeError :: Error -> Safe a
safeError = Left

---- Append Error

appendErrorTupleList :: [ErrorTuple] -> Safe a -> Safe a
appendErrorTupleList = appendError . tupleListToError

appendErrorTuple :: ErrorTuple -> Safe a -> Safe a
appendErrorTuple = appendError . tupleToError

appendError :: Error -> Safe a -> Safe a
appendError message = first (<> message)

----

tupleListToError :: [ErrorTuple] -> Error
tupleListToError xs = mconcat $ tupleToError <$> xs

tupleToError :: ErrorTuple -> Error
tupleToError (prefix , showed) = " [" <> format prefix <> showed <> "]" where
  format "" = ""
  format _  = prefix <> " "

----

unsafe :: Safe a -> a
unsafe (Right a) = a
unsafe (Left a) = error a

----

type SafeFail_ m = SafeFail m ()
type SafeFail m a = m (Safe a)

type SafeLegacy a = Either String a

type Safe_  = Safe ()
type Safe a = Either Error a

type ErrorTuple = (Error , Error)

type Error = Text
