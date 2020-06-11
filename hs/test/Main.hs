module Main where

import qualified Spec
import Test.Hspec.Slow
import Test.Hspec (hspec)

main :: IO ()
main = main' =<< configure 1 where
  main' config = hspec $ timeThese config Spec.spec
