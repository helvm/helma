module HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.Evaluator.LLEvaluator
import           HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData
import           HelVM.HelMA.Automata.WhiteSpace.FileUtil
import           HelVM.HelMA.Automata.WhiteSpace.Instruction
import           HelVM.HelMA.Automata.WhiteSpace.SimpleParams

import           HelVM.CartesianProduct
import           HelVM.GoldenExpectations

import           HelVM.HelMA.Automaton.IO.MockIO
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           System.FilePath.Posix

import           Test.Hspec                                            (Spec, describe, it)

spec :: Spec
spec = do
  describe "simpleEval" $ do
    let options =  [True , False]

    describe "ws" $ do
      describe "original" $ do
        let majorPath = "simpleEval" </> "original" </> "ws"
        forM_
          [ ("count"        , ""           )
          , ("hworld"       , ""           )
          , ("calc"         , "-1\n"       )
          , ("fact"         , "10\n"       )
          , ("hanoi"        , "1\n"        )
          , ("loctest"      , "1\n2\n"     )
          , ("name"         , "WriteOnly\n")
          ] $ \(fileName , input) -> do
            let file = readWsFile ("original" </> fileName)
            forM_ options $ \ ascii -> do
              let paramsF = simpleParamsWithDefaultsAndWhiteTokenType ascii
              let paramsIO = paramsF <$> file
              let mock = ioExecMockIOWithInput input . simpleEval =<< paramsIO
              let minorPath = show ascii </> fileName
              describe minorPath $ do
                it ("output" </> minorPath) $ do
                  calculateOutput <$> mock `goldenShouldIO` buildAbsoluteOutFileName (majorPath </> "output" </> minorPath)
                it ("logged" </> minorPath) $ do
                  calculateLogged <$> mock `goldenShouldIO` buildAbsoluteOutFileName (majorPath </> "logged" </> minorPath)

    describe "stn" $ do

      describe "from-wsa" $ do
        let majorPath = "simpleEval" </> "from-wsa" </> "stn"
        forM_
          [ "true"
          , "hello"
          , "hello2"
          , "hello4"
          , "bottles"
          , "prim"
          ] $ \ fileName -> do
            let file = readStnFile ("from-wsa" </> fileName)
            forM_ options $ \ ascii -> do
              let paramsF = simpleParamsWithDefaultsAndVisibleTokenType ascii
              let paramsIO = paramsF <$> file
              let mock = ioExecMockIOBatch . simpleEval =<< paramsIO
              let minorPath = show ascii </> fileName
              describe minorPath $ do
                it ("output" </> minorPath) $ do
                  calculateOutput <$> mock `goldenShouldIO` buildAbsoluteOutFileName (majorPath </> "output" </> minorPath)
                it ("logged" </> minorPath) $ do
                  calculateLogged <$> mock `goldenShouldIO` buildAbsoluteOutFileName (majorPath </> "logged" </> minorPath)

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
               ] >><| options) $ \(fileName , input , ascii) -> do
          let file = readStnFile ("original" </> fileName)
          let paramsF = simpleParamsWithDefaultsAndVisibleTokenType ascii
          let paramsIO = paramsF <$> file
          let mock = ioExecMockIOWithInput input . simpleEval =<< paramsIO
          let minorPath = show ascii </> fileName
          describe minorPath $ do
            it ("output" </> minorPath) $ do
              calculateOutput <$> mock `goldenShouldIO` buildAbsoluteOutFileName (majorPath </> "output" </> minorPath)
            it ("logged" </> minorPath) $ do
              calculateLogged <$> mock `goldenShouldIO` buildAbsoluteOutFileName (majorPath </> "logged" </> minorPath)

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
      let mock = (safeExecMockIOWithInput input . simpleEvalTL) tl
      describe fileName $ do
        it ("output" </> fileName) $ do
          calculateOutput <$> mock `goldenShouldSafe` buildAbsoluteOutFileName ("simpleEvalTL" </> "output" </> fileName)
        it ("logged" </> fileName) $ do
          calculateLogged <$> mock `goldenShouldSafe` buildAbsoluteOutFileName ("simpleEvalTL" </> "logged" </> fileName)

  describe "simpleEvalIL" $ do
    forM_ [ ("call"     , [Call "A", End , Mark "A", Return] , "")
          , ("push-pop" , [Liter 0 , Discard , End]          , "")
          ] $ \(fileName , il , input) -> do
      let mock = safeExecMockIOWithInput input $ evalIL il SeqStackType MapListRAMType
      it fileName $ do
        calculateLogged <$> mock `goldenShouldSafe` buildAbsoluteOutFileName ("simpleEvalIL" </> "logged" </> fileName)
