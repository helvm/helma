module Main where

import qualified Spec
import           Test.Hspec      (hspec)
import           Test.Hspec.Slow

main :: IO ()
main = main' =<< configure 2 where
  main' config = hspec $ timeThese config Spec.spec
