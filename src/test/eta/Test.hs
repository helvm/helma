module Main(main) where

import HelVM.HelCam.BrainFuck.TokensTest
import HelVM.HelCam.WhiteSpace.ParserTest

import Test.HUnit

testExample :: Test
testExample = TestCase (assertEqual "test" "test" "test")

testList :: Test
testList = TestList 
  [ TestLabel "testExample" testExample
  , TestLabel "testsOfTokens" testsOfTokens
  , TestLabel "testOfParser" testsOfParser
  ]

main :: IO ()
main = do
  _ <- runTestTT testList
  return ()
