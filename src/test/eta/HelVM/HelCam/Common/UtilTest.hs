module HelVM.HelCam.Common.UtilTest where

import HelVM.HelCam.Common.Util

import Test.HUnit

testsUtil :: Test
testsOfFilterIf0 = test
  [ "testFilter0"  ~: "test FilterIf0"  ~: "qwerty\n" ~=? execMockIO (mFilterIf0 mockGetChar mockPutChar) "qwerty0uiop"
  , "testWFilter0" ~: "test WFilterIf0" ~: "qwerty\n" ~=? execMockIO wFilterIf0 "qwerty0uiop"
  ]
