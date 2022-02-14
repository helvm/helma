module HelVM.Expectations  (
  ioShouldSafe,
  ioShouldBe,
  shouldControlT,
  shouldSafeT,
  shouldSafeIO,
  shouldSafe,
  shouldBeDo,
) where

import           HelVM.Common.Control.Control
import           HelVM.Common.Control.Safe

import           Test.Hspec.Expectations.Pretty

infix 1 `ioShouldSafe`
ioShouldSafe :: (Show a , Eq a) => IO (Safe a) -> IO a -> Expectation
ioShouldSafe action expected = join $ liftA2 shouldSafe action expected

infix 1 `ioShouldBe`
ioShouldBe :: (HasCallStack , Show a , Eq a) => IO a -> IO a -> Expectation
ioShouldBe action expected = join $ liftA2 shouldBe action expected

----

infix 1 `shouldControlT`
shouldControlT :: (Show a , Eq a) => ControlT IO a -> a -> Expectation
shouldControlT action = shouldReturn (controlTToIOWithLogs action)

infix 1 `shouldSafeT`
shouldSafeT :: (Show a , Eq a) => SafeT IO a -> a -> Expectation
shouldSafeT action = shouldReturn (safeTToIO action)

infix 1 `shouldSafeIO`
shouldSafeIO :: (Show a , Eq a) => IO (Safe a) -> a -> Expectation
shouldSafeIO action = shouldReturn (safeIOToIO action)

infix 1 `shouldSafe`
shouldSafe :: (HasCallStack , Show a , Eq a) => Safe a -> a -> Expectation
shouldSafe action expected = shouldBe action $ pure expected

infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack , Show a , Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected
