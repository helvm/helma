module HelVM.HelCam.BrainFuck.TokensTest where

import HelVM.HelCam.BrainFuck.EvaluatorTestData

import HelVM.HelCam.BrainFuck.Lexer

import Test.HUnit

testsOfBFTokens :: Test
testsOfBFTokens = test
  [ "tokenizeHelloWorld"             ~: "tokenize helloWorld"             ~: helloWorld       ~=? show (readTokens helloWorld)
  , "tokenizeHelloWorldWithComments" ~: "tokenize helloWorldWithComments" ~: helloWorld       ~=? show (readTokens helloWorldWithComments)
  , "testTokenAsList"                ~: "testTokenAsList"                 ~: helloWorldAsList ~=? show (tokenList $ readTokens helloWorldWithComments)
  ]

helloWorldAsList :: String
helloWorldAsList = "[+,+,+,+,+,+,+,+,[,>,+,+,+,+,[,>,+,+,>,+,+,+,>,+,+,+,>,+,<,<,<,<,-,],>,+,>,+,>,-,>,>,+,[,<,],<,-,],>,>,.,>,-,-,-,.,+,+,+,+,+,+,+,.,.,+,+,+,.,>,>,.,<,-,.,<,.,+,+,+,.,-,-,-,-,-,-,.,-,-,-,-,-,-,-,-,.,>,>,+,.,>,+,+,.]"
