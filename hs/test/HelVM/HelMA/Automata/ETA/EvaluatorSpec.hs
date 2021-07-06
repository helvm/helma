module HelVM.HelMA.Automata.ETA.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.ETA.Evaluator
import HelVM.HelMA.Automata.ETA.FileUtil

import HelVM.CartesianProduct
import HelVM.GoldenExpectations

import HelVM.HelMA.Automaton.IO.MockIO
import HelVM.HelMA.Automaton.Types.StackType

import System.FilePath.Posix

import Test.Hspec (Spec , describe , it)

spec :: Spec
spec = do
  describe "from-eas" $ do
    forM_ ([ ("true"    , ""   )
           , ("hello"   , ""   )
           , ("hello2"  , ""   )
           , ("hello3"  , ""   )
           , ("hello4"  , ""   )
           , ("readnum" , "0\n")
           , ("readnum" , "1\n")
           , ("fact"    , "0\n")
           , ("fact"    , "1\n")
           , ("fact"    , "2\n")
           , ("fact"    , "3\n")
           , ("fact"    , "4\n")
           , ("fact"    , "5\n")
           , ("fact"    , "6\n")
           , ("fact"    , "7\n")
           , ("fact"    , "8\n")
           , ("fact"    , "9\n")
--           , ("fact"    , "10\n")
           , ("bottles" , ""   )
           ] >><| stackTypes
          ) $ \(fileName , input , stackType) -> do
      let params = (, stackType) <$> readEtaFile ("from-eas" </> fileName)
      let mock = ioExecMockIOWithInput (toText input) . uncurryEval =<< params
      let minorPath = show stackType </> fileName <> input
      describe minorPath$ do
        it ("monadic" </> minorPath) $ do
          calculateOutput <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("from-eas" </> "monadic" </> minorPath)
        it ("logging" </> minorPath) $ do
          calculateLogged <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("from-eas" </> "logging" </> minorPath)

  describe "original" $ do
    forM_ ([ ("hello"   , "" )
           , ("hello2"  , "" )
--           , ("fact"    , "0\n" )
           , ("fact"    , "1\n" )
           , ("fact"    , "2\n" )
           , ("fact"    , "3\n" )
           , ("fact"    , "4\n" )
           , ("fact"    , "5\n" )
           , ("fact"    , "6\n" )
           , ("fact"    , "7\n" )
           , ("fact"    , "8\n" )
--           , ("fact"    , "9\n" )
           , ("bottles" , "" )
           , ("crlf"    , "" )
           ] >><| stackTypes
          ) $ \(fileName , input , stackType) -> do
      let params = (, stackType) <$> readEtaFile ("original" </> fileName)
      let mock = ioExecMockIOWithInput (toText input) . uncurryEval =<< params
      let minorPath = show stackType </> fileName <> input
      describe minorPath $ do
        it ("monadic" </> minorPath) $ do
          calculateOutput <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("original" </> "monadic" </> minorPath)
        it ("logging" </> minorPath) $ do
          calculateLogged <$> mock `goldenShouldIO` buildAbsoluteOutFileName ("original" </> "logging" </> minorPath)
