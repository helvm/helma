module HelVM.Common.Collections.MapListSpec (spec)  where

import           HelVM.Common.Collections.MapList

import qualified Data.ListLike                    as LL
import qualified GHC.Exts                         as I (IsList (..))

import           Test.Hspec                       (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "fromList" $
    forM_ [ ("[0]"       , [0]      )
          , ("[1]"       , [1]      )
          , ("[0,1]"     , [0,1]    )
          , ("[1,0]"     , [1,0]    )
          , ("[0,1,2,3]" , [0,1,2,3])
          , ("[3,2,1,0]" , [3,2,1,0])
          ] $ \(name , list) ->
      it name $ I.toList (fromList list :: MapList Int) `shouldBe` list

  describe "toDescList . fromIntIndexedList" $
    forM_ [ ([(0,0)]       , [(0,0)]      )
          , ([(0,1)]       , [(0,1)]      )
          , ([(1,0)]       , [(1,0)]      )
          , ([(1,1)]       , [(1,1)]      )
          , ([(2,0)]       , [(2,0)]      )
          , ([(2,1)]       , [(2,1)]      )
          , ([(2,2),(1,1)] , [(2,2),(1,1)])
          , ([(1,1),(2,2)] , [(2,2),(1,1)])
          , ([(5,2),(3,1)] , [(5,2),(3,1)])
          , ([(3,1),(5,2)] , [(5,2),(3,1)])
          ] $ \(input , output) ->
      it (show input) $ (toDescList . fromIntIndexedList) input  `shouldBe` output

  describe "fromIntIndexedList" $ do
    describe "desc" $
      forM_ [ ([(0,0)]       , [0]          )
            , ([(0,1)]       , [1]          )
            , ([(1,0)]       , [0,0]        )
            , ([(1,1)]       , [0,1]        )
            , ([(2,0)]       , [0,0,0]      )
            , ([(2,1)]       , [0,0,1]      )
            , ([(2,2),(1,1)] , [0,1,2]      )
            , ([(5,2),(3,1)] , [0,0,0,1,0,2])
            ] $ \(input , output) ->
        describe (show input) $ do
          it "               listFromIntDescList" $                 listFromIntDescList  input `shouldBe` output
          it "mapListToList . fromIntIndexedList" $ (mapListToList . fromIntIndexedList) input `shouldBe` output
          it "I.toList      . fromIntIndexedList" $ (I.toList      . fromIntIndexedList) input `shouldBe` output
          it "LL.toList     . fromIntIndexedList" $ (LL.toList     . fromIntIndexedList) input `shouldBe` output

    describe "asc" $
      forM_ [ ([(1,1),(2,2)] , [0,1,2]      )
            , ([(3,1),(5,2)] , [0,0,0,1,0,2])
            ] $ \(input , output) ->
        describe (show input) $ do
          it "mapListToList . fromIntIndexedList" $ (mapListToList . fromIntIndexedList) input `shouldBe` output
          it "I.toList      . fromIntIndexedList" $ (I.toList      . fromIntIndexedList) input `shouldBe` output
          it "LL.toList     . fromIntIndexedList" $ (LL.toList     . fromIntIndexedList) input `shouldBe` output

listFromIntDescList :: [(Int , Int)] -> [Int]
listFromIntDescList = listFromDescList

fromIntIndexedList :: [(Int , Int)] -> MapList Int
fromIntIndexedList = fromIndexedList
