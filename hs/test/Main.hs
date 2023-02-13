module Main where

import qualified Spec
import           Test.Hspec      (hspec)
import           Test.Hspec.Slow

main :: IO ()
main = (hspec . flip timeThese Spec.spec) =<< configure 2
