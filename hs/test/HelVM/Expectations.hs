module HelVM.Expectations  (
  ioShouldSafe,
  ioShouldBe,
  shouldSafeExceptT,
  shouldSafeIO,
  shouldSafe,
  shouldBeDo,
) where

import           HelVM.Common.Safe

import           Test.Hspec.Expectations.Pretty

-- | ioShould* functions
infix 1 `ioShouldSafe`
ioShouldSafe :: (Show a , Eq a) => IO (Safe a) -> IO a -> Expectation
ioShouldSafe action expected = join $ liftA2 shouldSafe action expected

infix 1 `ioShouldBe`
ioShouldBe :: (HasCallStack , Show a , Eq a) => IO a -> IO a -> Expectation
ioShouldBe action expected = join $ liftA2 shouldBe action expected

-- | shouldSafe* functions
infix 1 `shouldSafeExceptT`
shouldSafeExceptT :: (Show a , Eq a) => SafeExceptT IO a -> a -> Expectation
shouldSafeExceptT action = shouldReturn (exceptTToIO action)

infix 1 `shouldSafeIO`
shouldSafeIO :: (Show a , Eq a) => IO (Safe a) -> a -> Expectation
shouldSafeIO action = shouldReturn (safeIOToIO action)

infix 1 `shouldSafe`
shouldSafe :: (HasCallStack , Show a , Eq a) => Safe a -> a -> Expectation
shouldSafe action expected = shouldBe action $ pure expected

infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack , Show a , Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected
