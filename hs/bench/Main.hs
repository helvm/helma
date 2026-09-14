{-# LANGUAGE UnicodeSyntax #-}
module Main where

import qualified Spec
import           Test.Hspec (hspec)

main ∷ IO ()
main = hspec Spec.spec
