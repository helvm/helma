module HelVM.HelMA.Automata.ETA.EvaluatorUtilSpec (spec) where

import HelVM.HelMA.Automata.ETA.EvaluatorUtil
import HelVM.HelMA.Automata.ETA.EvaluatorSpecData
import HelVM.HelMA.Automata.ETA.Token

import Test.Hspec

spec :: Spec
spec = do
  describe "parseInteger" $ do
    it "[E]"          $ do parseInteger [E]          `shouldBe` 0
    it "[S , E]"       $ do parseInteger [S , E]       `shouldBe` 6
    it "[T , E]"       $ do parseInteger [T , E]       `shouldBe` 1
    it "[S , S , E]"    $ do parseInteger [S , S , E]    `shouldBe` 48
    it "[S , T , E]"    $ do parseInteger [S , T , E]    `shouldBe` 43
    it "[T , S , E]"    $ do parseInteger [T , S , E]    `shouldBe` 13
    it "[T , T , E]"    $ do parseInteger [T , T , E]    `shouldBe` 8
    it "[S , S , S , E]" $ do parseInteger [S , S , S , E] `shouldBe` 342
    it "[S , S , T , E]" $ do parseInteger [S , S , T , E] `shouldBe` 337
    it "[S , T , S , E]" $ do parseInteger [S , T , S , E] `shouldBe` 307
    it "[S , T , T , E]" $ do parseInteger [S , T , T , E] `shouldBe` 302
    it "[T , S , S , E]" $ do parseInteger [T , S , S , E] `shouldBe` 97
    it "[T , S , T , E]" $ do parseInteger [T , S , T , E] `shouldBe` 92
    it "[T , T , S , E]" $ do parseInteger [T , T , S , E] `shouldBe` 62
    it "[T , T , T , E]" $ do parseInteger [T , T , T , E] `shouldBe` 57

  describe "findAddress ertrar" $ do
    it "1" $ do findAddress ertrar 1 `shouldBe` 0
    it "2" $ do findAddress ertrar 2 `shouldBe` 2
    it "3" $ do findAddress ertrar 3 `shouldBe` 4
    it "4" $ do findAddress ertrar 4 `shouldBe` 6

  describe "nextLabel ertrar" $ do
    it "0" $ do nextLabel ertrar 0 `shouldBe` 2
    it "1" $ do nextLabel ertrar 1 `shouldBe` 2
    it "2" $ do nextLabel ertrar 2 `shouldBe` 3
    it "3" $ do nextLabel ertrar 3 `shouldBe` 3
    it "4" $ do nextLabel ertrar 4 `shouldBe` 4
    it "5" $ do nextLabel ertrar 5 `shouldBe` 4
    it "6" $ do nextLabel ertrar 6 `shouldBe` 5

  describe "findAddress etaretaretar" $ do
    it "1" $ do findAddress etaretaretar 1 `shouldBe` 0
    it "2" $ do findAddress etaretaretar 2 `shouldBe` 4
    it "3" $ do findAddress etaretaretar 3 `shouldBe` 8
    it "4" $ do findAddress etaretaretar 4 `shouldBe` 12


  describe "nextLabel etaretaretar" $ do
    it "0" $ do nextLabel etaretaretar 0 `shouldBe` 2
    it "1" $ do nextLabel etaretaretar 1 `shouldBe` 2
    it "2" $ do nextLabel etaretaretar 2 `shouldBe` 2
    it "3" $ do nextLabel etaretaretar 3 `shouldBe` 2
    it "4" $ do nextLabel etaretaretar 4 `shouldBe` 3
    it "5" $ do nextLabel etaretaretar 5 `shouldBe` 3
    it "6" $ do nextLabel etaretaretar 6 `shouldBe` 3

  describe "findAddress hello2TL" $ do
    it "1" $ do findAddress hello2TL 1 `shouldBe` 0
    it "2" $ do findAddress hello2TL 2 `shouldBe` 40
    it "3" $ do findAddress hello2TL 3 `shouldBe` 78
    it "4" $ do findAddress hello2TL 4 `shouldBe` 106
    it "5" $ do findAddress hello2TL 5 `shouldBe` 123

  describe "nextLabel hello2TL" $ do
    forM_ [ (38  , 2)
          , (76  , 3)
          , (78  , 4)
          , (106 , 5)
          , (123 , 6)
          ] $ \(input , output) -> do
      it (show input) $ do nextLabel hello2TL input `shouldBe` output

parseInteger :: TokenList -> Integer
parseInteger tl = fst $ parseNumber $ IU tl 0

ertrar :: TokenList
ertrar = [E , R , T , R , A , R]

etaretaretar :: TokenList
etaretaretar = [E , T , A , R , E , T , A , R , E , T , A , R]
