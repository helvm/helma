module HelVM.HelCam.Machines.ETA.LexerTest where

import HelVM.HelCam.Machines.ETA.EvaluatorTestData

import HelVM.HelCam.Machines.ETA.Lexer
import HelVM.HelCam.Machines.ETA.Token


import Test.HUnit

testsOfETATokens :: Test
testsOfETATokens = test
  [ "tokenize_EE"      ~: "The classic ``Hello, world!'' program." ~: ("E\n"::String)                          ~=? show ( WhiteTokens [WhiteToken E, WhiteToken R])
  , "tokenize_E"       ~: "The classic ``Hello, world!'' program." ~: WhiteTokens [WhiteToken E, WhiteToken R] ~=? readTokens "E\n"
  , "tokenize_hello"   ~: "Zip hello."                             ~: helloZIP                                 ~=? show ( readTokens helloETA)
  , "tokenize_hello"   ~: "Zip hello2."                            ~: hello2ZIP                                ~=? show ( readTokens hello2ETA)
  , "tokenize_hello"   ~: "Tokenize hello."                        ~: helloTL                                  ~=? tokenize helloETA
  , "tokenize_hello"   ~: "Tokenize hello2."                       ~: hello2TL                                 ~=? tokenize hello2ETA
--  , "test_hello2"  ~: "An alternative implementation of ``Hello, world!''."             ~: "8 bit cells\n"     ~=? batchExecMockIO (evalWord8 value256)
--  , "test_pip"     ~: "Copy standard input to standard output."                         ~: helloWorldExpected  ~=? batchExecMockIO (evalWord8 helloWorld)
--  , "test_pip2"    ~: "A smaller copy-input-to-output program."                          ~: helloWorldExpected  ~=? batchExecMockIO (evalWord8 fascistHelloWorld)
--  , "test_fact"    ~: "Print the recursively-calculated factorial of the number on the standard input stream."  ~: hello_WorldExpected ~=? batchExecMockIO (evalWord8 padHelloWorld)
--  , "test_cottles" ~: "Print the lyrics to ``99 Bottles of Beer on the Wall''."          ~: hello_WorldExpected ~=? batchExecMockIO (evalWord8 theShortestHelloWorld)
--  , "test_crlf"    ~: "Test whether the interpreter handles CR/LF sequences correctly."  ~: hello_WorldExpected ~=? batchExecMockIO (evalWord8 theShortestHelloWorld)
  ]
