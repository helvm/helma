module HelVM.HelCam.Machines.ETA.EvaluatorUtilTest where

import HelVM.HelCam.Machines.ETA.EvaluatorUtil
import HelVM.HelCam.Machines.ETA.EvaluatorTestData
import HelVM.HelCam.Machines.ETA.Token

import Test.HUnit

testsOfETAEvaluatorUtil :: Test
testsOfETAEvaluatorUtil = TestList
  [ "testParseNatural_"    ~: "testParseNatural_"    ~:   0 ~=? parseInteger [E]
  , "testParseNatural_S"   ~: "testParseNatural_S"   ~:   6 ~=? parseInteger [S, E]
  , "testParseNatural_T"   ~: "testParseNatural_T"   ~:   1 ~=? parseInteger [T, E]
  , "testParseNatural_SS"  ~: "testParseNatural_SS"  ~:  48 ~=? parseInteger [S, S, E]
  , "testParseNatural_ST"  ~: "testParseNatural_ST"  ~:  43 ~=? parseInteger [S, T, E]
  , "testParseNatural_TS"  ~: "testParseNatural_TS"  ~:  13 ~=? parseInteger [T, S, E]
  , "testParseNatural_TT"  ~: "testParseNatural_TT"  ~:   8 ~=? parseInteger [T, T, E]
  , "testParseNatural_SSS" ~: "testParseNatural_SSS" ~: 342 ~=? parseInteger [S, S, S, E]
  , "testParseNatural_SST" ~: "testParseNatural_SST" ~: 337 ~=? parseInteger [S, S, T, E]
  , "testParseNatural_STS" ~: "testParseNatural_STS" ~: 307 ~=? parseInteger [S, T, S, E]
  , "testParseNatural_STT" ~: "testParseNatural_STT" ~: 302 ~=? parseInteger [S, T, T, E]
  , "testParseNatural_TSS" ~: "testParseNatural_TSS" ~:  97 ~=? parseInteger [T, S, S, E]
  , "testParseNatural_TST" ~: "testParseNatural_TST" ~:  92 ~=? parseInteger [T, S, T, E]
  , "testParseNatural_TTS" ~: "testParseNatural_TTS" ~:  62 ~=? parseInteger [T, T, S, E]
  , "testParseNatural_TTT" ~: "testParseNatural_TTT" ~:  57 ~=? parseInteger [T, T, T, E]

  , "test_findAddress" ~: "test findAddress"     ~:   0 ~=?  findAddress ertrar 1
  , "test_findAddress" ~: "test findAddress"     ~:   2 ~=?  findAddress ertrar 2
  , "test_findAddress" ~: "test findAddress"     ~:   4 ~=?  findAddress ertrar 3
  , "test_findAddress" ~: "test findAddress"     ~:   6 ~=?  findAddress ertrar 4

  , "test_nextLabel"  ~: "testNextLineNumber 0" ~:   2 ~=?  nextLabel ertrar 0
  , "test_nextLabel"  ~: "testNextLineNumber 1" ~:   2 ~=?  nextLabel ertrar 1
  , "test_nextLabel"  ~: "testNextLineNumber 2" ~:   3 ~=?  nextLabel ertrar 2
  , "test_nextLabel"  ~: "testNextLineNumber 3" ~:   3 ~=?  nextLabel ertrar 3
  , "test_nextLabel"  ~: "testNextLineNumber 4" ~:   4 ~=?  nextLabel ertrar 4
  , "test_nextLabel"  ~: "testNextLineNumber 5" ~:   4 ~=?  nextLabel ertrar 5
  , "test_nextLabel"  ~: "testNextLineNumber 6" ~:   5 ~=?  nextLabel ertrar 6
  
  , "test_findAddress" ~: "test findAddress 1"     ~:   0 ~=?  findAddress etaretaretar 1
  , "test_findAddress" ~: "test findAddress 2"     ~:   4 ~=?  findAddress etaretaretar 2
  , "test_findAddress" ~: "test findAddress 3"     ~:   8 ~=?  findAddress etaretaretar 3
  , "test_findAddress" ~: "test findAddress 4"     ~:   12 ~=? findAddress etaretaretar 4

  , "test_nextLabel"  ~: "testNextLineNumber 0" ~:   2 ~=?  nextLabel etaretaretar 0
  , "test_nextLabel"  ~: "testNextLineNumber 1" ~:   2 ~=?  nextLabel etaretaretar 1
  , "test_nextLabel"  ~: "testNextLineNumber 2" ~:   2 ~=?  nextLabel etaretaretar 2
  , "test_nextLabel"  ~: "testNextLineNumber 3" ~:   2 ~=?  nextLabel etaretaretar 3
  , "test_nextLabel"  ~: "testNextLineNumber 4" ~:   3 ~=?  nextLabel etaretaretar 4
  , "test_nextLabel"  ~: "testNextLineNumber 5" ~:   3 ~=?  nextLabel etaretaretar 5
  , "test_nextLabel"  ~: "testNextLineNumber 6" ~:   3 ~=?  nextLabel etaretaretar 6
  
  , "test_hello"  ~: "test 1 findAddress" ~:       0 ~=? findAddress hello2TL 1
  , "test_hello"  ~: "test 1 nextLabel"   ~:       2 ~=? nextLabel hello2TL 38

  , "test_hello"  ~: "test 2 findAddress" ~:      40 ~=? findAddress hello2TL 2
  , "test_hello"  ~: "test 2 nextLabel"   ~:       3 ~=? nextLabel hello2TL 76

  , "test_hello"  ~: "test 3 findAddress" ~:      78 ~=? findAddress hello2TL 3
  , "test_hello"  ~: "test 3 nextLabel"   ~:       4 ~=? nextLabel hello2TL 78

  , "test_hello"  ~: "test 4 findAddress" ~:     106 ~=? findAddress hello2TL 4
  , "test_hello"  ~: "test 4 nextLabel"   ~:       5 ~=? nextLabel hello2TL 106

  , "test_hello"  ~: "test 5 findAddress" ~:    123 ~=? findAddress hello2TL 5
  , "test_hello"  ~: "test 5 nextLabel"   ~:      6 ~=? nextLabel hello2TL 123

  ]

parseInteger :: TokenList -> Integer
parseInteger tl = fst $ parseNumber $ IU tl 0

ertrar :: TokenList
ertrar = [E, R, T, R, A, R]

etaretaretar :: TokenList
etaretaretar = [E, T, A, R, E, T, A, R, E, T, A, R]

