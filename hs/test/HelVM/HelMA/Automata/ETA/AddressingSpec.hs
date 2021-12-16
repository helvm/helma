module HelVM.HelMA.Automata.ETA.AddressingSpec (spec) where

import           HelVM.HelMA.Automata.ETA.Addressing
import           HelVM.HelMA.Automata.ETA.EvaluatorSpecData
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.Expectations

import qualified Data.Vector                                as Vector

import           Test.Hspec                                 (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "findAddress ertrar" $ do
    it "1" $ findAddress ertrar 1 `shouldSafe` 0
    it "2" $ findAddress ertrar 2 `shouldSafe` 2
    it "3" $ findAddress ertrar 3 `shouldSafe` 4
    it "4" $ findAddress ertrar 4 `shouldSafe` 6

  describe "nextLabel ertrar" $ do
    it "0" $ nextLabel ertrar 0 `shouldBe` 2
    it "1" $ nextLabel ertrar 1 `shouldBe` 2
    it "2" $ nextLabel ertrar 2 `shouldBe` 3
    it "3" $ nextLabel ertrar 3 `shouldBe` 3
    it "4" $ nextLabel ertrar 4 `shouldBe` 4
    it "5" $ nextLabel ertrar 5 `shouldBe` 4
    it "6" $ nextLabel ertrar 6 `shouldBe` 5

  describe "findAddress etaretaretar" $ do
    it "1" $ findAddress etaretaretar 1 `shouldSafe` 0
    it "2" $ findAddress etaretaretar 2 `shouldSafe` 4
    it "3" $ findAddress etaretaretar 3 `shouldSafe` 8
    it "4" $ findAddress etaretaretar 4 `shouldSafe` 12

  describe "nextLabel etaretaretar" $ do
    it "0" $ nextLabel etaretaretar 0 `shouldBe` 2
    it "1" $ nextLabel etaretaretar 1 `shouldBe` 2
    it "2" $ nextLabel etaretaretar 2 `shouldBe` 2
    it "3" $ nextLabel etaretaretar 3 `shouldBe` 2
    it "4" $ nextLabel etaretaretar 4 `shouldBe` 3
    it "5" $ nextLabel etaretaretar 5 `shouldBe` 3
    it "6" $ nextLabel etaretaretar 6 `shouldBe` 3

  describe "findAddress hello2TL" $ do
    it "1" $ findAddress hello2TL 1 `shouldSafe` 0
    it "2" $ findAddress hello2TL 2 `shouldSafe` 40
    it "3" $ findAddress hello2TL 3 `shouldSafe` 78
    it "4" $ findAddress hello2TL 4 `shouldSafe` 106
    it "5" $ findAddress hello2TL 5 `shouldSafe` 123

  describe "nextLabel hello2TL" $
    forM_ [ (38  , 2)
          , (76  , 3)
          , (78  , 4)
          , (106 , 5)
          , (123 , 6)
          ] $ \(input , output) ->
      it (show input) $ nextLabel hello2TL input `shouldBe` output

ertrar :: TokenVector
ertrar = Vector.fromList [E , R , T , R , A , R]

etaretaretar :: TokenVector
etaretaretar = Vector.fromList [E , T , A , R , E , T , A , R , E , T , A , R]
