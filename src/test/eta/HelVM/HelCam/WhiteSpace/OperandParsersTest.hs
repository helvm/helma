module HelVM.HelCam.WhiteSpace.OperandParsersTest where

import HelVM.HelCam.WhiteSpace.OperandParsers
import HelVM.HelCam.WhiteSpace.Token

import Test.HUnit

testsOfWSOperandParsers :: Test
testsOfWSOperandParsers = TestList
  [ TestLabel "testOfParseInteger" testOfParseInteger
  , TestLabel "testOfParseNatural" testOfParseNatural
  , TestLabel "testOfParseBitString" testOfParseBitString
  ]

----

testOfParseNatural :: Test
testOfParseNatural = test
  [ "testParseNatural_"    ~: "testParseNatural_"    ~: 0 ~=? fst ( parseNatural [N])
  , "testParseNatural_S"   ~: "testParseNatural_S"   ~: 0 ~=? fst ( parseNatural [S, N])
  , "testParseNatural_T"   ~: "testParseNatural_T"   ~: 1 ~=? fst ( parseNatural [T, N])
  , "testParseNatural_SS"  ~: "testParseNatural_SS"  ~: 0 ~=? fst ( parseNatural [S, S, N])
  , "testParseNatural_ST"  ~: "testParseNatural_ST"  ~: 1 ~=? fst ( parseNatural [S, T, N])
  , "testParseNatural_TS"  ~: "testParseNatural_TS"  ~: 2 ~=? fst ( parseNatural [T, S, N])
  , "testParseNatural_TT"  ~: "testParseNatural_TT"  ~: 3 ~=? fst ( parseNatural [T, T, N])
  , "testParseNatural_SSS" ~: "testParseNatural_SSS" ~: 0 ~=? fst ( parseNatural [S, S, S, N])
  , "testParseNatural_SST" ~: "testParseNatural_SST" ~: 1 ~=? fst ( parseNatural [S, S, T, N])
  , "testParseNatural_STS" ~: "testParseNatural_STS" ~: 2 ~=? fst ( parseNatural [S, T, S, N])
  , "testParseNatural_STT" ~: "testParseNatural_STT" ~: 3 ~=? fst ( parseNatural [S, T, T, N])
  , "testParseNatural_TSS" ~: "testParseNatural_TSS" ~: 4 ~=? fst ( parseNatural [T, S, S, N])
  , "testParseNatural_TST" ~: "testParseNatural_TST" ~: 5 ~=? fst ( parseNatural [T, S, T, N])
  , "testParseNatural_TTS" ~: "testParseNatural_TTS" ~: 6 ~=? fst ( parseNatural [T, T, S, N])
  , "testParseNatural_TTT" ~: "testParseNatural_TTT" ~: 7 ~=? fst ( parseNatural [T, T, T, N])
  ]

testOfParseInteger :: Test
testOfParseInteger = test
  [ "testParseInteger_"    ~: "testParseInteger_"    ~:   0  ~=? fst ( parseInteger [N])
  , "testParseInteger_S"   ~: "testParseInteger_S"   ~:   0  ~=? fst ( parseInteger [S, N])
  , "testParseInteger_T"   ~: "testParseInteger_T"   ~:   0  ~=? fst ( parseInteger [T, N])
  , "testParseInteger_SS"  ~: "testParseInteger_SS"  ~:   0  ~=? fst ( parseInteger [S, S, N])
  , "testParseInteger_ST"  ~: "testParseInteger_ST"  ~:   1  ~=? fst ( parseInteger [S, T, N])
  , "testParseInteger_TS"  ~: "testParseInteger_TS"  ~:   0  ~=? fst ( parseInteger [T, S, N])
  , "testParseInteger_TT"  ~: "testParseInteger_TT"  ~: (-1) ~=? fst ( parseInteger [T, T, N])
  , "testParseInteger_SSS" ~: "testParseInteger_SSS" ~:   0  ~=? fst ( parseInteger [S, S, S, N])
  , "testParseInteger_SST" ~: "testParseInteger_SST" ~:   1  ~=? fst ( parseInteger [S, S, T, N])
  , "testParseInteger_STS" ~: "testParseInteger_STS" ~:   2  ~=? fst ( parseInteger [S, T, S, N])
  , "testParseInteger_STT" ~: "testParseInteger_STT" ~:   3  ~=? fst ( parseInteger [S, T, T, N])
  , "testParseInteger_TSS" ~: "testParseInteger_TSS" ~:   0  ~=? fst ( parseInteger [T, S, S, N])
  , "testParseInteger_TST" ~: "testParseInteger_TST" ~: (-1) ~=? fst ( parseInteger [T, S, T, N])
  , "testParseInteger_TTS" ~: "testParseInteger_TTS" ~: (-2) ~=? fst ( parseInteger [T, T, S, N])
  , "testParseInteger_TTT" ~: "testParseInteger_TTT" ~: (-3) ~=? fst ( parseInteger [T, T, T, N])
  ]

testOfParseBitString :: Test
testOfParseBitString = test
  [ "testParseBitString_SSS" ~: "testParseBitString_SSS" ~: "000" ~=? fst ( parseBitString [S, S, S, N])
  , "testParseBitString_SST" ~: "testParseBitString_SST" ~: "001" ~=? fst ( parseBitString [S, S, T, N])
  , "testParseBitString_STS" ~: "testParseBitString_STS" ~: "010" ~=? fst ( parseBitString [S, T, S, N])
  , "testParseBitString_STT" ~: "testParseBitString_STT" ~: "011" ~=? fst ( parseBitString [S, T, T, N])
  , "testParseBitString_TSS" ~: "testParseBitString_TSS" ~: "100" ~=? fst ( parseBitString [T, S, S, N])
  , "testParseBitString_TST" ~: "testParseBitString_TST" ~: "101" ~=? fst ( parseBitString [T, S, T, N])
  , "testParseBitString_TTS" ~: "testParseBitString_TTS" ~: "110" ~=? fst ( parseBitString [T, T, S, N])
  , "testParseBitString_TTT" ~: "testParseBitString_TTT" ~: "111" ~=? fst ( parseBitString [T, T, T, N])
  ]
