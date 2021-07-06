module HelVM.Common.Safe (
  safeIOToPTextIO,

  safeIOToIO,
  safeToIO,
  exceptTToIO,
  safeExceptT,
  safeToEitherLegacy,
  errorsToText,

  liftExceptT,
  liftSafe,
  liftEitherError,
  liftEitherLegacy,

  liftMaybeOrErrorTupleList,
  liftMaybeOrErrorTuple,
  liftMaybeOrError,

  liftErrorTupleList,
  liftErrorTuple,
  liftError,

  appendErrorTupleList,
  appendErrorTuple,
  appendError,

  tupleListToError,
  tupleToError,

  MonadSafeError,
  SafeExceptT,
  EitherLegacy,
  EitherError,
  Safe,
  Error,
) where

import HelVM.Common.Util

import Control.Monad.Except hiding (ExceptT , runExceptT)

import System.IO.Error

import qualified Data.List.Singleton as List

safeIOToPTextIO :: Show a => IO (Safe a) -> IO Text
safeIOToPTextIO a = showP <$> safeIOToIO a

safeIOToIO :: IO (Safe a) -> IO a
safeIOToIO a = safeToIO =<< a

safeToIO :: Safe a -> IO a
safeToIO = exceptTToIO . liftSafe

exceptTToIO :: SafeExceptT IO a -> IO a
exceptTToIO = liftExceptT . withExceptT (userError . errorsToString)

safeExceptT :: Monad m => m a -> SafeExceptT m a
safeExceptT a = ExceptT $ pure <$> a

safeToEitherLegacy :: Safe a -> EitherLegacy a
safeToEitherLegacy = first errorsToString

errorsToString :: Errors -> String
errorsToString = toString . errorsToText

errorsToText :: Errors -> Text
errorsToText = unlines . reverse

-- Lift

liftExceptT :: MonadError e m => ExceptT e m a -> m a
liftExceptT m = liftEither =<< runExceptT m

liftSafe :: MonadSafeError m => Safe a -> m a
liftSafe = liftEither

liftEitherError :: MonadSafeError m => Either Text a -> m a
liftEitherError = liftEither . first List.singleton

liftEitherLegacy :: MonadSafeError m => EitherLegacy a -> m a
liftEitherLegacy = liftSafe . first (List.singleton . toText)

-- Lift from Maybe

liftMaybeOrErrorTupleList :: MonadSafeError m => [ErrorTuple] -> Maybe a -> m a
liftMaybeOrErrorTupleList = liftMaybeOrError . tupleListToError

liftMaybeOrErrorTuple :: MonadSafeError m => ErrorTuple -> Maybe a -> m a
liftMaybeOrErrorTuple = liftMaybeOrError . tupleToError

liftMaybeOrError :: MonadSafeError m => Error -> Maybe a -> m a
liftMaybeOrError e = liftSafe . maybeToRight [e]

-- Lift from Error

liftErrorTupleList :: MonadSafeError m => [ErrorTuple] -> m a
liftErrorTupleList = liftError . tupleListToError

liftErrorTuple :: MonadSafeError m => ErrorTuple -> m a
liftErrorTuple = liftError . tupleToError

liftError :: MonadSafeError m => Error -> m a
liftError e = throwError [e]

-- Append Error

appendErrorTupleList :: MonadSafeError m => [ErrorTuple] -> m a -> m a
appendErrorTupleList = appendError . tupleListToError

appendErrorTuple :: MonadSafeError m => ErrorTuple -> m a -> m a
appendErrorTuple = appendError . tupleToError

appendError :: MonadSafeError m => Error -> m a -> m a
appendError message a = catchError a appendAndThrow where appendAndThrow es = throwError ([message] <> es)

----

tupleListToError :: [ErrorTuple] -> Error
tupleListToError xs = mconcat $ tupleToError <$> xs

tupleToError :: ErrorTuple -> Error
tupleToError (prefix , showed) = " [" <> format prefix <> showed <> "]" where
  format "" = ""
  format _  = prefix <> " "

----

type MonadSafeError m = MonadError Errors m

type SafeExceptT m = ExceptT Errors m

type EitherLegacy = Either String

type EitherError = Either Text

type Safe = Either Errors

type ErrorTuple = (Error , Error)

type Errors = [Text]

type Error = Text
