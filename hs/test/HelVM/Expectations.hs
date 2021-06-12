module HelVM.Expectations where

import HelVM.Common.Safe

import Test.Hspec

infix 1 `ioShouldBe`
ioShouldBe :: (HasCallStack , Show a , Eq a) => IO a -> IO a -> Expectation
ioShouldBe action expected = join $ liftA2 shouldBe action expected

infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack , Show a , Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected

infix 1 `shouldSafe`
shouldSafe :: (HasCallStack , Show a , Eq a) => Safe a -> a -> Expectation
shouldSafe action expected = shouldBe action $ safe expected
