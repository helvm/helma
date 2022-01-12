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
  describe "findAddress ertrar" $
    forM_ [ (1 , 0)
          , (2 , 2)
          , (3 , 4)
          , (4 , 6)
          ] $ \(input , output) ->
      it (show input) $ findAddress ertrar input `shouldSafe` output

  describe "nextLabel ertrar" $
    forM_ [ (0 , 2)
          , (1 , 2)
          , (2 , 3)
          , (3 , 3)
          , (4 , 4)
          , (5 , 4)
          , (6 , 5)
          ] $ \(input , output) ->
      it (show input) $ nextLabel ertrar input `shouldBe` output

  describe "findAddress etaretaretar" $
    forM_ [ (1 , 0)
          , (2 , 4)
          , (3 , 8)
          , (4 , 12)
          ] $ \(input , output) ->
      it (show input) $ findAddress etaretaretar input `shouldSafe` output

  describe "nextLabel etaretaretar" $
    forM_ [ (0 , 2)
          , (1 , 2)
          , (2 , 2)
          , (3 , 2)
          , (4 , 3)
          , (5 , 3)
          , (6 , 3)
          ] $ \(input , output) ->
      it (show input) $ nextLabel etaretaretar input `shouldBe` output

  describe "findAddress hello2TL" $
    forM_ [ (1 , 0)
          , (2 , 40)
          , (3 , 78)
          , (4 , 106)
          , (5 , 123)
          ] $ \(input , output) ->
      it (show input) $ findAddress hello2TL input `shouldSafe` output

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
