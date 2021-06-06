module HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.Evaluator
import HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData
import HelVM.HelMA.Automata.WhiteSpace.FileUtil
import HelVM.HelMA.Automata.WhiteSpace.Instruction

import HelVM.HelMA.Automata.CartesianProduct
import HelVM.HelMA.Automata.Expectations

import HelVM.HelMA.Common.IO.MockIO

import HelVM.HelMA.Common.Types.RAMType
import HelVM.HelMA.Common.Types.StackType
import HelVM.HelMA.Common.Types.TokenType

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "simpleEval" $ do
    let options =  [True , False] |><< stackTypes |><| ramTypes
    describe "ws" $ do
      describe "original" $ do
        let majorPath = "simpleEval" </> "original" </> "ws"
        forM_ ([ ("count"        , ""           )
               , ("hworld"       , ""           )
               , ("calc"         , "-1\n"       )
               , ("fact"         , "10\n"       )
               , ("hanoi"        , "1\n"        )
               , ("loctest"      , "1\n2\n"     )
               , ("name"         , "WriteOnly\n")
               ] >><<< options) $ \(fileName , input , ascii , stackType , ramType) -> do
          let minorPath = show ascii <-> show stackType <-> show ramType </> fileName
          let params = (WhiteTokenType ,  , ascii , stackType , ramType) <$> readWsFile ("original" </> fileName)
          describe minorPath $ do
            it ("interact" </> minorPath) $ do
              flipSimpleEval input              <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "interact" </> minorPath)
            it ("monadic"  </> minorPath) $ do
              flipExecMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "monadic"  </> minorPath)
            it ("logging"  </> minorPath) $ do
              flipEvalMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "logging"  </> minorPath)

    describe "stn" $ do

      describe "from-wsa" $ do
        let majorPath = "simpleEval" </> "from-wsa" </> "stn"
        forM_ ([ ("true"        , ""           )
               , ("hello"       , ""           )
               , ("hello2"      , ""           )
               , ("hello4"      , ""           )
               , ("bottles"     , ""           )
               , ("prim"        , ""           )
               ] >><<< options) $ \(fileName , input , ascii , stackType , ramType) -> do
          let minorPath = show ascii <-> show stackType <-> show ramType </> fileName
          let params = (VisibleTokenType ,  , ascii , stackType , ramType) <$> readStnFile ("from-wsa" </> fileName)
          describe minorPath $ do
            it ("interact" </> minorPath) $ do
              flipSimpleEval input              <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "interact" </> minorPath)
            it ("monadic"  </> minorPath) $ do
              flipExecMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "monadic"  </> minorPath)
            it ("logging"  </> minorPath) $ do
              flipEvalMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "logging"  </> minorPath)

      describe "original" $ do
        let majorPath = "simpleEval" </> "original" </> "stn"
        forM_ ([ ("count"        , ""           )
               , ("helloWorld"   , ""           )
               , ("hWorld"       , ""           )
               , ("calc"         , "-1\n"       )
               , ("fact"         , "10\n"       )
               , ("hanoi"        , "1\n"        )
               , ("locTest"      , "1\n2\n"     )
               , ("name"         , "WriteOnly\n")
               , ("truthMachine" , "0\n"        )
               ] >><<< options) $ \(fileName , input , ascii , stackType , ramType) -> do
          let minorPath = show ascii <-> show stackType <-> show ramType </> fileName
          let params = (VisibleTokenType ,  , ascii , stackType , ramType) <$> readStnFile ("original" </> fileName)
          describe minorPath $ do
            it ("interact" </> minorPath) $ do
              flipSimpleEval input              <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "interact" </> minorPath)
            it ("monadic"  </> minorPath) $ do
              flipExecMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "monadic"  </> minorPath)
            it ("logging"  </> minorPath) $ do
              flipEvalMockIO input . simpleEval <$> params `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "logging"  </> minorPath)

  describe "simpleEvalTL" $ do
    forM_ [ ("countTL"        , countTL        , ""           )
          , ("helloWorldTL"   , helloWorldTL   , ""           )
          , ("hWorldTL"       , hWorldTL       , ""           )
          , ("calcTL"         , calcTL         , "-1\n"       )
          , ("factTL"         , factTL         , "10\n"       )
          , ("hanoiTL"        , hanoiTL        , "1\n"        )
          , ("locTestTL"      , locTestTL      , "1\n2\n"     )
          , ("nameTL"         , nameTL         , "WriteOnly\n")
          , ("truthMachineTL" , truthMachineTL , "0\n"        )
          ] $ \(fileName , tl , input) -> do
      describe fileName $ do
        it ("interact" </> fileName) $ do
          flipSimpleEvalTL input                 tl `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalTL" </> "interact" </> fileName)
        it ("monadic"  </> fileName) $ do
          (flipExecMockIO  input . simpleEvalTL) tl `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalTL" </> "monadic"  </> fileName)
        it ("logging"  </> fileName) $ do
          (flipEvalMockIO  input . simpleEvalTL) tl `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalTL" </> "logging"  </> fileName)

  describe "simpleEvalIL" $ do
    forM_ [ ("call"     , [Call "A", End , Mark "A", Return] , "")
          , ("push-pop" , [Liter 0 , Discard , End]          , "")
          , ("pop"      , [Discard , End]                    , "")
          ] $ \(fileName , il , input) -> do
      it fileName $ do
        flipEvalMockIO input (evalIL il SeqStackType IntMapRAMType) `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalIL" </> "logging"  </> fileName)
