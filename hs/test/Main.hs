module Main where

import           Slow
import qualified Spec
import           Test.Hspec (hspec)

main :: IO ()
main = (hspec . flip timeThese Spec.spec) =<< configure 1
