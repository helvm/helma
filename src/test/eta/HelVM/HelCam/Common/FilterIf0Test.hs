module HelVM.HelCam.Common.FilterIf0Test where

import HelVM.HelCam.Common.WrapperIO
import HelVM.HelCam.Common.MockIO

import Test.HUnit

wFilterIf0 :: WrapperIO m => m ()
wFilterIf0 = do
  char <- wGetChar
  if char == '0'
    then do 
      wLogStrLn ""
      wPutChar '\n'
    else do
      wPutChar char
      wFilterIf0

testsOfFilterIf0 :: Test
testsOfFilterIf0 = test
  [ "testWFilter0" ~: "test WFilterIf0" ~: "qwerty\n" ~=? execMockIO wFilterIf0 "qwerty0uiop"
  , "testWFilter0" ~: "test WFilterIf0" ~: "\n"       ~=? evalMockIO wFilterIf0 "qwerty0uiop"
  ]
