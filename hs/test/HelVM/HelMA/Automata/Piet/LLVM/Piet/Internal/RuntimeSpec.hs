{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.RuntimeSpec
  ( main
  , spec
  ) where

import           Control.Monad
import qualified HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Runtime as P
import qualified LLVM.AST                                             as AST
import qualified LLVM.AST.Type                                        as T
import           LLVM.IRBuilder
import           LLVMTestUtils
import           Test.Hspec

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "commands" $ do
    let pushRollOut xs = mapM_ P.push xs >> P.roll >> mapM_ (const P.outNumber) xs
    forM_
      [ ("pop", \_ -> P.pop, "", "", 0, 0)

      , ("push 10; outNumber", \_ -> P.push 10 >> P.outNumber, "", "10", 0, 0)

      , ("add; outNumber", \_ -> P.add >> P.outNumber, "", "", 0, 0)
      , ("push 10; add; outNumber", \_ -> P.push 10 >> P.add >> P.outNumber, "", "10", 0, 0)
      , ("push 10; push 2; add; outNumber", \_ -> P.push 10 >> P.push 2 >> P.add >> P.outNumber, "", "12", 0, 0)

      , ("subtract; outNumber", \_ -> P.subtract >> P.outNumber, "", "", 0, 0)
      , ("push 10; subtract; outNumber", \_ -> P.push 10 >> P.subtract >> P.outNumber, "", "10", 0, 0)
      , ("push 10; push 2; subtract; outNumber", \_ -> P.push 10 >> P.push 2 >> P.subtract >> P.outNumber, "", "8", 0, 0)

      , ("multiply; outNumber", \_ -> P.multiply >> P.outNumber, "", "", 0, 0)
      , ("push 10; multiply; outNumber", \_ -> P.push 10 >> P.multiply >> P.outNumber, "", "10", 0, 0)
      , ("push 10; push 2; multiply; outNumber", \_ -> P.push 10 >> P.push 2 >> P.multiply >> P.outNumber, "", "20", 0, 0)

      , ("divide; outNumber", \_ -> P.divide >> P.outNumber, "", "", 0, 0)
      , ("push 10; divide; outNumber", \_ -> P.push 10 >> P.divide >> P.outNumber, "", "10", 0, 0)
      , ("push 11; push 3; divide; outNumber", \_ -> P.push 11 >> P.push 3 >> P.divide >> P.outNumber, "", "3", 0, 0)
      , ("push 12; push 3; divide; outNumber", \_ -> P.push 12 >> P.push 3 >> P.divide >> P.outNumber, "", "4", 0, 0)
      , ("push 13; push 3; divide; outNumber", \_ -> P.push 13 >> P.push 3 >> P.divide >> P.outNumber, "", "4", 0, 0)
      , ("push -11; push 3; divide; outNumber", \_ -> P.push (-11) >> P.push 3 >> P.divide >> P.outNumber, "", "-4", 0, 0)
      , ("push -12; push 3; divide; outNumber", \_ -> P.push (-12) >> P.push 3 >> P.divide >> P.outNumber, "", "-4", 0, 0)
      , ("push -13; push 3; divide; outNumber", \_ -> P.push (-13) >> P.push 3 >> P.divide >> P.outNumber, "", "-5", 0, 0)
      , ("push 11; push -3; divide; outNumber", \_ -> P.push 11 >> P.push (-3) >> P.divide >> P.outNumber, "", "-4", 0, 0)
      , ("push 12; push -3; divide; outNumber", \_ -> P.push 12 >> P.push (-3) >> P.divide >> P.outNumber, "", "-4", 0, 0)
      , ("push 13; push -3; divide; outNumber", \_ -> P.push 13 >> P.push (-3) >> P.divide >> P.outNumber, "", "-5", 0, 0)
      , ("push -11; push -3; divide; outNumber", \_ -> P.push (-11) >> P.push (-3) >> P.divide >> P.outNumber, "", "3", 0, 0)
      , ("push -12; push -3; divide; outNumber", \_ -> P.push (-12) >> P.push (-3) >> P.divide >> P.outNumber, "", "4", 0, 0)
      , ("push -13; push -3; divide; outNumber", \_ -> P.push (-13) >> P.push (-3) >> P.divide >> P.outNumber, "", "4", 0, 0)
      , ("push 12; push 0; divide; outNumber", \_ -> P.push 12 >> P.push 0 >> P.divide >> P.outNumber >> P.outNumber, "", "012", 0, 0)

      , ("mod; outNumber", \_ -> P.mod >> P.outNumber, "", "", 0, 0)
      , ("push 10; mod; outNumber", \_ -> P.push 10 >> P.mod >> P.outNumber, "", "10", 0, 0)
      , ("push 11; push 3; mod; outNumber", \_ -> P.push 11 >> P.push 3 >> P.mod >> P.outNumber, "", "2", 0, 0)
      , ("push 12; push 3; mod; outNumber", \_ -> P.push 12 >> P.push 3 >> P.mod >> P.outNumber, "", "0", 0, 0)
      , ("push 13; push 3; mod; outNumber", \_ -> P.push 13 >> P.push 3 >> P.mod >> P.outNumber, "", "1", 0, 0)
      , ("push -11; push 3; mod; outNumber", \_ -> P.push (-11) >> P.push 3 >> P.mod >> P.outNumber, "", "1", 0, 0)
      , ("push -12; push 3; mod; outNumber", \_ -> P.push (-12) >> P.push 3 >> P.mod >> P.outNumber, "", "0", 0, 0)
      , ("push -13; push 3; mod; outNumber", \_ -> P.push (-13) >> P.push 3 >> P.mod >> P.outNumber, "", "2", 0, 0)
      , ("push 11; push -3; mod; outNumber", \_ -> P.push 11 >> P.push (-3) >> P.mod >> P.outNumber, "", "-1", 0, 0)
      , ("push 12; push -3; mod; outNumber", \_ -> P.push 12 >> P.push (-3) >> P.mod >> P.outNumber, "", "0", 0, 0)
      , ("push 13; push -3; mod; outNumber", \_ -> P.push 13 >> P.push (-3) >> P.mod >> P.outNumber, "", "-2", 0, 0)
      , ("push -11; push -3; mod; outNumber", \_ -> P.push (-11) >> P.push (-3) >> P.mod >> P.outNumber, "", "-2", 0, 0)
      , ("push -12; push -3; mod; outNumber", \_ -> P.push (-12) >> P.push (-3) >> P.mod >> P.outNumber, "", "0", 0, 0)
      , ("push -13; push -3; mod; outNumber", \_ -> P.push (-13) >> P.push (-3) >> P.mod >> P.outNumber, "", "-1", 0, 0)
      , ("push 12; push 0; mod; outNumber", \_ -> P.push 12 >> P.push 0 >> P.mod >> P.outNumber >> P.outNumber, "", "012", 0, 0)

      , ("not; outNumber", \_ -> P.not >> P.outNumber, "", "", 0, 0)
      , ("push -10; not; outNumber", \_ -> P.push (-10) >> P.not >> P.outNumber, "", "0", 0, 0)
      , ("push 0; not; outNumber", \_ -> P.push 0 >> P.not >> P.outNumber, "", "1", 0, 0)
      , ("push 10; not; outNumber", \_ -> P.push 10 >> P.not >> P.outNumber, "", "0", 0, 0)

      , ("greater; outNumber", \_ -> P.greater >> P.outNumber, "", "", 0, 0)
      , ("push 10; greater; outNumber", \_ -> P.push 10 >> P.greater >> P.outNumber, "", "10", 0, 0)
      , ("push 1; push 2; greater; outNumber", \_ -> P.push 1 >> P.push 2 >> P.greater >> P.outNumber, "", "0", 0, 0)
      , ("push 1; push 1; greater; outNumber", \_ -> P.push 1 >> P.push 1 >> P.greater >> P.outNumber, "", "0", 0, 0)
      , ("push 2; push 1; greater; outNumber", \_ -> P.push 2 >> P.push 1 >> P.greater >> P.outNumber, "", "1", 0, 0)

      , ("pointer; outNumber", \dpcc -> P.pointer dpcc, "", "", 0, 0)
      , ("push 1; push 1; pointer; outNumber", \dpcc -> P.push 1 >> P.push 1 >> P.pointer dpcc >> P.outNumber, "", "1", 0, 2)
      , ("push -4; pointer", \dpcc -> P.push (-4) >> P.pointer dpcc, "", "", 0, 0)
      , ("push -3; pointer", \dpcc -> P.push (-3) >> P.pointer dpcc, "", "", 0, 2)
      , ("push -2; pointer", \dpcc -> P.push (-2) >> P.pointer dpcc, "", "", 0, 4)
      , ("push -1; pointer", \dpcc -> P.push (-1) >> P.pointer dpcc, "", "", 0, 6)
      , ("push 0; pointer", \dpcc -> P.push 0 >> P.pointer dpcc, "", "", 0, 0)
      , ("push 1; pointer", \dpcc -> P.push 1 >> P.pointer dpcc, "", "", 0, 2)
      , ("push 2; pointer", \dpcc -> P.push 2 >> P.pointer dpcc, "", "", 0, 4)
      , ("push 3; pointer", \dpcc -> P.push 3 >> P.pointer dpcc, "", "", 0, 6)
      , ("push 4; pointer", \dpcc -> P.push 4 >> P.pointer dpcc, "", "", 0, 0)
      , ("push -4; pointer", \dpcc -> P.push (-4) >> P.pointer dpcc, "", "", 1, 1)
      , ("push -3; pointer", \dpcc -> P.push (-3) >> P.pointer dpcc, "", "", 1, 3)
      , ("push -2; pointer", \dpcc -> P.push (-2) >> P.pointer dpcc, "", "", 1, 5)
      , ("push -1; pointer", \dpcc -> P.push (-1) >> P.pointer dpcc, "", "", 1, 7)
      , ("push 0; pointer", \dpcc -> P.push 0 >> P.pointer dpcc, "", "", 1, 1)
      , ("push 1; pointer", \dpcc -> P.push 1 >> P.pointer dpcc, "", "", 1, 3)
      , ("push 2; pointer", \dpcc -> P.push 2 >> P.pointer dpcc, "", "", 1, 5)
      , ("push 3; pointer", \dpcc -> P.push 3 >> P.pointer dpcc, "", "", 1, 7)
      , ("push 4; pointer", \dpcc -> P.push 4 >> P.pointer dpcc, "", "", 1, 1)
      , ("push -4; pointer", \dpcc -> P.push (-4) >> P.pointer dpcc, "", "", 2, 2)
      , ("push -3; pointer", \dpcc -> P.push (-3) >> P.pointer dpcc, "", "", 2, 4)
      , ("push -2; pointer", \dpcc -> P.push (-2) >> P.pointer dpcc, "", "", 2, 6)
      , ("push -1; pointer", \dpcc -> P.push (-1) >> P.pointer dpcc, "", "", 2, 0)
      , ("push 0; pointer", \dpcc -> P.push 0 >> P.pointer dpcc, "", "", 2, 2)
      , ("push 1; pointer", \dpcc -> P.push 1 >> P.pointer dpcc, "", "", 2, 4)
      , ("push 2; pointer", \dpcc -> P.push 2 >> P.pointer dpcc, "", "", 2, 6)
      , ("push 3; pointer", \dpcc -> P.push 3 >> P.pointer dpcc, "", "", 2, 0)
      , ("push 4; pointer", \dpcc -> P.push 4 >> P.pointer dpcc, "", "", 2, 2)

      , ("switch; outNumber", \dpcc -> P.switch dpcc, "", "", 0, 0)
      , ("push 1; push 1; switch; outNumber", \dpcc -> P.push 1 >> P.push 1 >> P.switch dpcc >> P.outNumber, "", "1", 0, 1)
      , ("push -2; switch", \dpcc -> P.push (-2) >> P.switch dpcc, "", "", 0, 0)
      , ("push -1; switch", \dpcc -> P.push (-1) >> P.switch dpcc, "", "", 0, 1)
      , ("push 0; switch", \dpcc -> P.push 0 >> P.switch dpcc, "", "", 0, 0)
      , ("push 1; switch", \dpcc -> P.push 1 >> P.switch dpcc, "", "", 0, 1)
      , ("push 2; switch", \dpcc -> P.push 2 >> P.switch dpcc, "", "", 0, 0)
      , ("push -2; switch", \dpcc -> P.push (-2) >> P.switch dpcc, "", "", 1, 1)
      , ("push -1; switch", \dpcc -> P.push (-1) >> P.switch dpcc, "", "", 1, 0)
      , ("push 0; switch", \dpcc -> P.push 0 >> P.switch dpcc, "", "", 1, 1)
      , ("push 1; switch", \dpcc -> P.push 1 >> P.switch dpcc, "", "", 1, 0)
      , ("push 2; switch", \dpcc -> P.push 2 >> P.switch dpcc, "", "", 1, 1)
      , ("push -2; switch", \dpcc -> P.push (-2) >> P.switch dpcc, "", "", 2, 2)
      , ("push -1; switch", \dpcc -> P.push (-1) >> P.switch dpcc, "", "", 2, 3)
      , ("push 0; switch", \dpcc -> P.push 0 >> P.switch dpcc, "", "", 2, 2)
      , ("push 1; switch", \dpcc -> P.push 1 >> P.switch dpcc, "", "", 2, 3)
      , ("push 2; switch", \dpcc -> P.push 2 >> P.switch dpcc, "", "", 2, 2)

      , ("duplicate; outNumber", \_ -> P.duplicate >> P.outNumber, "", "", 0, 0)
      , ("push 1; duplicate; outNumber", \_ -> P.push 1 >> P.duplicate >> P.outNumber >> P.outNumber >> P.outNumber, "", "11", 0, 0)

      , ("roll; outNumber", \_ -> P.roll >> P.outNumber, "", "", 0, 0)
      , ("push 1; roll; outNumber", \_ -> P.push 1 >> P.roll >> P.outNumber, "", "1", 0, 0)
      , ("push [1, 0]; roll; outChar*", \_ -> pushRollOut [0, 0], "", "", 0, 0)
      , ("push [1, 0]; roll; outChar*", \_ -> pushRollOut [1, 0], "", "01", 0, 0)
      , ("push [1, 1]; roll; outChar*", \_ -> pushRollOut [1, 1], "", "11", 0, 0)
      , ("push [1, 2]; roll; outChar*", \_ -> pushRollOut [1, 2], "", "21", 0, 0)
      , ("push [3, 0, 0]; roll; outChar*", \_ -> pushRollOut [3, 0, 0], "", "3", 0, 0)
      , ("push [3, 0, 1]; roll; outChar*", \_ -> pushRollOut [3, 0, 1], "", "3", 0, 0)
      , ("push [3, 1, 0]; roll; outChar*", \_ -> pushRollOut [3, 1, 0], "", "3", 0, 0)
      , ("push [3, 1, 1]; roll; outChar*", \_ -> pushRollOut [3, 1, 1], "", "3", 0, 0)
      , ("push [3, 2, 1]; roll; outChar*", \_ -> pushRollOut [3, 2, 1], "", "123", 0, 0)
      , ("push [3, -1, 1]; roll; outChar*", \_ -> pushRollOut [3, -1, 1], "", "1-13", 0, 0)
      , ("push [9, 8, 7, 6, 0, 0]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 0, 0], "", "6789", 0, 0)
      , ("push [9, 8, 7, 6, 3, -10]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, -10], "", "8679", 0, 0)
      , ("push [9, 8, 7, 6, 3, -4]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, -4], "", "8679", 0, 0)
      , ("push [9, 8, 7, 6, 3, -3]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, -3], "", "6789", 0, 0)
      , ("push [9, 8, 7, 6, 3, -2]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, -2], "", "7869", 0, 0)
      , ("push [9, 8, 7, 6, 3, -1]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, -1], "", "8679", 0, 0)
      , ("push [9, 8, 7, 6, 3, 0]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, 0], "", "6789", 0, 0)
      , ("push [9, 8, 7, 6, 3, 1]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, 1], "", "7869", 0, 0)
      , ("push [9, 8, 7, 6, 3, 2]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, 2], "", "8679", 0, 0)
      , ("push [9, 8, 7, 6, 3, 3]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, 3], "", "6789", 0, 0)
      , ("push [9, 8, 7, 6, 3, 4]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, 4], "", "7869", 0, 0)
      , ("push [9, 8, 7, 6, 3, 10]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 3, 10], "", "7869", 0, 0)
      , ("push [9, 8, 7, 6, 5, 4]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 5, 4], "", "456789", 0, 0)
      , ("push [9, 8, 7, 6, 5, 5]; roll; outChar*", \_ -> pushRollOut [9, 8, 7, 6, 5, 5], "", "556789", 0, 0)

      , ("inNumber 10; outNumber", \_ -> P.inNumber >> P.outNumber, "10", "10", 0, 0)
      , ("inNumber x; outNumber", \_ -> P.inNumber >> P.outNumber, "x", "", 0, 0)
      , ("inNumber; outNumber", \_ -> P.inNumber >> P.outNumber, "", "", 0, 0)

      , ("inChar A; outNumber", \_ -> P.inChar >> P.outNumber, "A", "65", 0, 0)
      , ("inChar 10; outNumber", \_ -> P.inChar >> P.outNumber, "10", "49", 0, 0)
      , ("inChar; outNumber", \_ -> P.inChar >> P.outNumber, "", "", 0, 0)

      , ("push 65; outChar", \_ -> P.push 65 >> P.outChar >> P.outNumber, "", "A", 0, 0)
      ] $ \(commandDescription, command, input, expectedOutput, arg, expectedRetVal) ->
        context ("when given commands: " ++ commandDescription) $ do
          (actualRetVal, actualOutput) <- runIO $ runAndCapture (testAST command) "f" (arg, input)
          it "puts a correct string into stdout" $ actualOutput `shouldBe` expectedOutput
          it "returns a correct value" $ actualRetVal `shouldBe` expectedRetVal

testAST ∷ (AST.Operand → IRBuilderT ModuleBuilder ()) → AST.Module
testAST f = buildModule "" $ do
  P.declareFunctions
  _ <- function "f" [(T.i32, "x")] T.i32 $ \[defaultDpcc] -> do
    dpccPtr <- alloca T.i32 Nothing 4
    store dpccPtr 4 defaultDpcc
    f dpccPtr
    P.resetStack
    dpcc <- load dpccPtr 4
    ret dpcc
  return ()
