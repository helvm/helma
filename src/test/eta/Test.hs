module Main(main) where

import HelVM.HelCam.BrainFuck.TokensTest

import Test.HUnit

testExample :: Test
testExample = TestCase (assertEqual "test" "test" "test")

testList :: Test
testList = TestList [ TestLabel "testExample" testExample
                    , TestLabel "testsOfTokens" testsOfTokens
                    ]

main :: IO ()
main = do
  _ <- runTestTT testList
  return ()
