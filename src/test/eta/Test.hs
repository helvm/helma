module Main(main) where

import Test.HUnit

testExample :: Test
testExample = TestCase (assertEqual "test" "test" "test")

testList :: Test
testList = TestList [ TestLabel "testExample" testExample
                    ]

main :: IO ()
main = do
  _ <- runTestTT testList
  return ()
