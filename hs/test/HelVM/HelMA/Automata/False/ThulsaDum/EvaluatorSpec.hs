module HelVM.HelMA.Automata.False.ThulsaDum.EvaluatorSpec where

import           HelVM.HelMA.Automata.False.Evaluator.LLEvaluator

import           HelVM.HelMA.Automata.False.FileExtra

import           HelVM.HelMA.Automata.False.API.Version

import           HelVM.HelMA.Automaton.IO.MockIO

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec =
  forM_ [ ("thunderseethe" </> "prog"                       , "" )
        , ("strlen"        </> "primeNumber"                , "" ) -- too long
        , ("strlen"        </> "factorial"                  , "1\n" ) -- error
--        , ("strlen"        </> "examples"  </> "addcr"      , "1\n" ) -- error
        , ("strlen"        </> "examples"  </> "helloworld" , "" )
        , ("strlen"        </> "examples"  </> "lambda"     , "" ) -- error
        , ("strlen"        </> "examples"  </> "prime"     , "" )
        , ("strlen"        </> "examples"  </> "argtest"     , "2" )
        , ("strlen" </> "contrib" </> "Herb_Wollman" </> "Fibonacci" , "")
        , ("morphett" </> "factorial" , "")
        , ("morphett" </> "factorial2" , "")
        , ("morphett" </> "reverse" , "HelMA\n")
        , ("helma"         </> "add"       , "" )
        ] $ \(fileName , input) -> do
    let file = readFFile fileName
    let mock = (ioExecMockIOWithInput input . eval ThulsaDum) =<< file
    describe fileName $ do
      it "output" $
         (calculateOutput <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("ThulsaDum" </> "output" </> fileName)
      it "logged" $
         (calculateLogged <$> mock) `goldenShouldIO` buildAbsoluteOutFileName ("ThulsaDum" </> "logged" </> fileName)
