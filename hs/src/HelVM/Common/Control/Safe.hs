module HelVM.Common.Control.Safe (
  safeIOToPTextIO,

  safeIOToIO,
  safeToIO,
  safeTToIO,
  runSafeT,
  runSafe,

  safeToText,
  safeToEitherLegacy,

  orErrorTuple,
  orError,
  unsafe,

  maybeOrError,
  maybeToSafe,
  safeT,

  liftExceptT,
  liftSafe,
  liftEitherError,
  liftEitherLegacy,

  liftMaybeOrErrorTupleList,
  liftMaybeOrErrorTuple,
  liftMaybeOrError,

  liftErrorWithTupleList,
  liftErrorTupleList,
  liftErrorWithPrefix,
  liftErrorTuple,
  liftError,

  appendErrorTupleList,
  appendErrorTuple,
  appendError,

  MonadSafe,
  SafeT,
  EitherLegacy,
  EitherError,
  Safe,
) where

import           HelVM.Common.Control.Message

import           HelVM.Common.Util

import           Control.Monad.Except         hiding (ExceptT, runExceptT)

import           System.IO.Error

import qualified Data.DList                   as D

-- | DeConstructors
safeIOToPTextIO :: Show a => IO (Safe a) -> IO Text
safeIOToPTextIO a = showP <$> safeIOToIO a

safeIOToIO :: IO (Safe a) -> IO a
safeIOToIO a = safeToIO =<< a

safeToIO :: Safe a -> IO a
safeToIO = safeTToIO . liftSafe

safeTToIO :: SafeT IO a -> IO a
safeTToIO = liftExceptT . withExceptT (userError . errorsToString)

runSafeT :: SafeT m a -> m (Safe a)
runSafeT = runExceptT

runSafe :: Safe a -> Safe a
runSafe = id

safeToText :: Safe a -> Text
safeToText (Left messages) = errorsToText messages
safeToText (Right       _) = ""

safeToEitherLegacy :: Safe a -> EitherLegacy a
safeToEitherLegacy = first errorsToString

orErrorTuple :: MessageTuple -> Safe a -> a
orErrorTuple t = unsafe . appendErrorTuple t

orError :: Show e => e -> Safe a -> a
orError e = unsafe . appendError (show e)

unsafe :: Safe a -> a
unsafe (Right a) = a
unsafe (Left  a) = (error . errorsToText) a

-- | Constructors
maybeOrError :: Show e => e -> Maybe a -> Safe a
maybeOrError = maybeToSafe . show

maybeToSafe :: Message -> Maybe a -> Safe a
maybeToSafe = maybeToRight . D.singleton

safeT :: Monad m => m a -> SafeT m a
safeT a = ExceptT $ pure <$> a

-- | Lift
liftExceptT :: MonadError e m => ExceptT e m a -> m a
liftExceptT m = liftEither =<< runExceptT m

liftSafe :: MonadSafe m => Safe a -> m a
liftSafe = liftEither

liftEitherError :: MonadSafe m => EitherError a -> m a
liftEitherError = liftSafe . first D.singleton

liftEitherLegacy :: MonadSafe m => EitherLegacy a -> m a
liftEitherLegacy = liftSafe . first stringToErrors

-- | Lift from Maybe
liftMaybeOrErrorTupleList :: MonadSafe m => [MessageTuple] -> Maybe a -> m a
liftMaybeOrErrorTupleList = liftMaybeOrError . tupleListToMessage

liftMaybeOrErrorTuple :: MonadSafe m => MessageTuple -> Maybe a -> m a
liftMaybeOrErrorTuple = liftMaybeOrError . tupleToMessage

liftMaybeOrError :: MonadSafe m => Message -> Maybe a -> m a
liftMaybeOrError e = liftSafe . maybeToRight (D.singleton e)

-- | Lift from Message
liftErrorWithTupleList :: MonadSafe m => Message -> [MessageTuple] -> m a
liftErrorWithTupleList m l = liftError (m <> tupleListToMessage l)

liftErrorTupleList :: MonadSafe m => [MessageTuple] -> m a
liftErrorTupleList = liftError . tupleListToMessage

liftErrorWithPrefix :: MonadSafe m => Message -> Message -> m a
liftErrorWithPrefix prefix showed = liftErrorTuple (prefix , showed)

liftErrorTuple :: MonadSafe m => MessageTuple -> m a
liftErrorTuple = liftError . tupleToMessage

liftError :: MonadSafe m => Message -> m a
liftError = throwError . D.singleton

-- | Append Message
appendErrorTupleList :: MonadSafe m => [MessageTuple] -> m a -> m a
appendErrorTupleList = appendError . tupleListToMessage

appendErrorTuple :: MonadSafe m => MessageTuple -> m a -> m a
appendErrorTuple = appendError . tupleToMessage

appendError :: MonadSafe m => Message -> m a -> m a
appendError message a = catchError a appendAndThrow where appendAndThrow es = throwError (es `D.snoc` message)

-- | Types
type MonadSafe m = MonadError Messages m

type SafeT m = ExceptT Messages m

type Safe = Either Messages

type EitherError = Either Text

type EitherLegacy = Either String
