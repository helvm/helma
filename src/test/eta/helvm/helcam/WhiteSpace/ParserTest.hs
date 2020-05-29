module HelVM.HelCam.WhiteSpace.ParserTest where

import HelVM.HelCam.WhiteSpace.Parser
import HelVM.HelCam.WhiteSpace.Token
import HelVM.HelCam.WhiteSpace.Instruction

import Numeric.Natural

import Test.HUnit hiding (Label)

---

testOfParseNatural :: Test
testOfParseNatural = TestList 
  [ TestLabel "testParseNatural_"    (TestCase (assertEqual "testParseNatural_"    0 (fst $ parseNatural [N])))
  , TestLabel "testParseNatural_S"   (TestCase (assertEqual "testParseNatural_S"   0 (fst $ parseNatural [S, N])))
  , TestLabel "testParseNatural_T"   (TestCase (assertEqual "testParseNatural_T"   1 (fst $ parseNatural [T, N])))
  , TestLabel "testParseNatural_SS"  (TestCase (assertEqual "testParseNatural_SS"  0 (fst $ parseNatural [S, S, N])))
  , TestLabel "testParseNatural_ST"  (TestCase (assertEqual "testParseNatural_ST"  1 (fst $ parseNatural [S, T, N])))
  , TestLabel "testParseNatural_TS"  (TestCase (assertEqual "testParseNatural_TS"  2 (fst $ parseNatural [T, S, N])))
  , TestLabel "testParseNatural_TT"  (TestCase (assertEqual "testParseNatural_TT"  3 (fst $ parseNatural [T, T, N])))
  , TestLabel "testParseNatural_SSS" (TestCase (assertEqual "testParseNatural_SSS" 0 (fst $ parseNatural [S, S, S, N])))
  , TestLabel "testParseNatural_SST" (TestCase (assertEqual "testParseNatural_SST" 1 (fst $ parseNatural [S, S, T, N])))
  , TestLabel "testParseNatural_STS" (TestCase (assertEqual "testParseNatural_STS" 2 (fst $ parseNatural [S, T, S, N])))
  , TestLabel "testParseNatural_STT" (TestCase (assertEqual "testParseNatural_STT" 3 (fst $ parseNatural [S, T, T, N])))
  , TestLabel "testParseNatural_TSS" (TestCase (assertEqual "testParseNatural_TSS" 4 (fst $ parseNatural [T, S, S, N])))
  , TestLabel "testParseNatural_TST" (TestCase (assertEqual "testParseNatural_TST" 5 (fst $ parseNatural [T, S, T, N])))
  , TestLabel "testParseNatural_TTS" (TestCase (assertEqual "testParseNatural_TTS" 6 (fst $ parseNatural [T, T, S, N])))
  , TestLabel "testParseNatural_TTT" (TestCase (assertEqual "testParseNatural_TTT" 7 (fst $ parseNatural [T, T, T, N])))
  ]

testOfParseInteger :: Test
testOfParseInteger = TestList 
  [ TestLabel "testParseInteger_"    (TestCase (assertEqual "testParseInteger_"      0  (fst $ parseInteger [N])))
  , TestLabel "testParseInteger_S"   (TestCase (assertEqual "testParseInteger_S"     0  (fst $ parseInteger [S, N])))
  , TestLabel "testParseInteger_T"   (TestCase (assertEqual "testParseInteger_T"     0  (fst $ parseInteger [T, N])))
  , TestLabel "testParseInteger_SS"  (TestCase (assertEqual "testParseInteger_SS"    0  (fst $ parseInteger [S, S, N])))
  , TestLabel "testParseInteger_ST"  (TestCase (assertEqual "testParseInteger_ST"    1  (fst $ parseInteger [S, T, N])))
  , TestLabel "testParseInteger_TS"  (TestCase (assertEqual "testParseInteger_TS"    0  (fst $ parseInteger [T, S, N])))
  , TestLabel "testParseInteger_TT"  (TestCase (assertEqual "testParseInteger_TT"  (-1) (fst $ parseInteger [T, T, N])))
  , TestLabel "testParseInteger_SSS" (TestCase (assertEqual "testParseInteger_SSS"   0  (fst $ parseInteger [S, S, S, N])))
  , TestLabel "testParseInteger_SST" (TestCase (assertEqual "testParseInteger_SST"   1  (fst $ parseInteger [S, S, T, N])))
  , TestLabel "testParseInteger_STS" (TestCase (assertEqual "testParseInteger_STS"   2  (fst $ parseInteger [S, T, S, N])))
  , TestLabel "testParseInteger_STT" (TestCase (assertEqual "testParseInteger_STT"   3  (fst $ parseInteger [S, T, T, N])))
  , TestLabel "testParseInteger_TSS" (TestCase (assertEqual "testParseInteger_TSS"   0  (fst $ parseInteger [T, S, S, N])))
  , TestLabel "testParseInteger_TST" (TestCase (assertEqual "testParseInteger_TST" (-1) (fst $ parseInteger [T, S, T, N])))
  , TestLabel "testParseInteger_TTS" (TestCase (assertEqual "testParseInteger_TTS" (-2) (fst $ parseInteger [T, T, S, N])))
  , TestLabel "testParseInteger_TTT" (TestCase (assertEqual "testParseInteger_TTT" (-3) (fst $ parseInteger [T, T, T, N])))
  ]

catTokenList :: TokenList
catTokenList = [
  N,S,S,S,N,
  S,S,S,T,N,
  T,N,T,S,
  S,S,S,T,N,
  T,T,T,
  T,N,S,S,
  S,S,S,T,N,
  N,T,S,T,N,
  N,S,N,S,N,
  N,S,S,T,N,
  N,N,N]

catIL :: InstructionList
catIL = [
  Label 0,
  Const 1, InputChar, 
  Const 1, Load, OutputChar,
  Const 1,
  Branch EZ 1,
  Jump 0,

  Label 1,
  End]

helloWorldTokenList :: TokenList
helloWorldTokenList = [
  S,S,S,T,S,S,T,S,S,S,N,
  T,N,S,S,
  S,S,S,T,T,S,S,T,S,T,N,
  T,N,S,S,
  S,S,S,T,T,S,T,T,S,S,N,
  T,N,S,S,
  S,S,S,T,T,S,T,T,S,S,N,
  T,N,S,S,
  S,S,S,T,T,S,T,T,T,T,N,
  T,N,S,S,
  S,S,S,T,S,T,T,S,S,N,
  T,N,S,S,
  S,S,S,T,S,S,S,S,S,N,
  T,N,S,S,
  S,S,S,T,T,T,S,T,T,T,N,
  T,N,S,S,
  S,S,S,T,T,S,T,T,T,T,N,
  T,N,S,S,
  S,S,S,T,T,T,S,S,T,S,N,
  T,N,S,S,
  S,S,S,T,T,S,T,T,S,S,N,
  T,N,S,S,
  S,S,S,T,T,S,S,T,S,S,N,
  T,N,S,S,
  N,N,N]
                
helloWorldIL :: InstructionList
helloWorldIL = [
  Const 72, OutputChar,
  Const 101, OutputChar,
  Const 108, OutputChar,
  Const 108, OutputChar,
  Const 111,OutputChar,
  Const 44, OutputChar,
  Const 32, OutputChar,
  Const 119, OutputChar,
  Const 111, OutputChar,
  Const 114, OutputChar,
  Const 108, OutputChar,
  Const 100, OutputChar,
  End]

truthMachineTokenList :: TokenList
truthMachineTokenList = [S,S,S,N,
  S,N,S,
  T,N,T,T,
  T,T,T,
  N,T,S,S,N,
  N,S,S,T,N,
  S,S,S,T,N,
  T,N,S,T,
  N,S,N,T,N,
  N,S,S,S,N,
  S,S,S,N,
  T,N,S,T,
  N,N,N]

truthMachineIL :: InstructionList
truthMachineIL = [
  Const 0,
  Dup,
  InputNum,
  Load,
  Branch EZ 0,
  
  Label 1,
  Const 1, OutputNum,
  Jump 1,
  
  Label 0,
  Const 0, OutputNum,
  End]

testOfParseCat :: Test
testOfParseCat = TestCase (assertEqual "cat" catIL (parse catTokenList))

testOfParseHelloWorld :: Test
testOfParseHelloWorld = TestCase (assertEqual "helloWorld" helloWorldIL (parse helloWorldTokenList))

testOfParseTruthMachine :: Test
testOfParseTruthMachine = TestCase (assertEqual "testOfParseTruthMachine" truthMachineIL (parse truthMachineTokenList))

testsOfParser :: Test
testsOfParser = TestList 
  [ TestLabel "testOfParseInteger" testOfParseInteger
  , TestLabel "testOfParseNatural" testOfParseNatural
  , TestLabel "testOfParseCat" testOfParseCat
  , TestLabel "testOfParseHelloWorld" testOfParseHelloWorld
  , TestLabel "testOfParseTruthMachine" testOfParseTruthMachine
  ]
