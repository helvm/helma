module HelVM.HelCam.Common.FilterIf0Test where

import HelVM.HelCam.Common.FilterIf0
import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfFilterIf0 :: Test
testsOfFilterIf0 = test
  [ "testFilter0"  ~: "test FilterIf0"  ~: "qwerty\n" ~=? execMockIO (mFilterIf0 mockGetChar mockPutChar) "qwerty0uiop"
  , "testWFilter0" ~: "test WFilterIf0" ~: "qwerty\n" ~=? execMockIO wFilterIf0 "qwerty0uiop"
  ]
