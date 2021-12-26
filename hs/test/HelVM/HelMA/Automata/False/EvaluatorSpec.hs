module HelVM.HelMA.Automata.False.EvaluatorSpec where

import           HelVM.HelMA.Automata.False.Evaluator.LLEvaluator

import           HelVM.HelMA.Automata.False.FileUtil

import           HelVM.HelMA.Automaton.IO.MockIO

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do
  forM_ [ ("thunderseethe" </> "prog"                       , "" )
--        , ("strlen"        </> "primeNumber"                , "" ) -- too long
--        , ("strlen"        </> "factorial"                  , "1\n" ) -- error
--        , ("strlen"        </> "examples"  </> "addcr"      , "1\n" ) -- error
        , ("strlen"        </> "examples"  </> "helloworld" , "" )
--        , ("strlen"        </> "examples"  </> "lambda"     , "" ) -- error
        , ("strlen"        </> "examples"  </> "prime"     , "" )
        , ("strlen"        </> "examples"  </> "argtest"     , "2" )
        , ("strlen/contrib/Herb_Wollman/Fibonacci" , "")
        , ("morphett/factorial" , "")
--        , ("morphett/factorial2" , "")
--        , ("morphett/reverse" , "HelMA\n")
        , ("helma"         </> "add"       , "" )
        ] $ \(fileName , input) -> do
    let file = readFFile fileName
    let mock = (ioExecMockIOWithInput input . eval) =<< file
    describe fileName $ do
      it "output" $ do
         (calculateOutput <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("output" </> fileName)
      it "logged" $ do
         (calculateLogged <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("logged" </> fileName)
