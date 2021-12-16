module HelVM.Common.Collections.MapListSpec (spec)  where

import           HelVM.Common.Collections.MapList

import qualified Data.ListLike                    as LL
import qualified GHC.Exts                         as I (IsList (..))

import           Test.Hspec                       (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "fromList" $ do
    it "[0]"       $ I.toList (fromList [0]       :: MapList Int) `shouldBe` [0]
    it "[1]"       $ I.toList (fromList [1]       :: MapList Int) `shouldBe` [1]
    it "[0,1]"     $ I.toList (fromList [0,1]     :: MapList Int) `shouldBe` [0,1]
    it "[1,0]"     $ I.toList (fromList [1,0]     :: MapList Int) `shouldBe` [1,0]
    it "[0,1,2,3]" $ I.toList (fromList [0,1,2,3] :: MapList Int) `shouldBe` [0,1,2,3]
    it "[3,2,1,0]" $ I.toList (fromList [3,2,1,0] :: MapList Int) `shouldBe` [3,2,1,0]

  describe "listFromIntDescList" $ do
    it "[(0,0)]"       $ listFromIntDescList [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ listFromIntDescList [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ listFromIntDescList [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ listFromIntDescList [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ listFromIntDescList [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ listFromIntDescList [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ listFromIntDescList [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ listFromIntDescList [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]

  describe "toDescList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ (toDescList . fromIntIndexedList) [(0,0)]       `shouldBe` [(0,0)]
    it "[(0,1)]"       $ (toDescList . fromIntIndexedList) [(0,1)]       `shouldBe` [(0,1)]
    it "[(1,0)]"       $ (toDescList . fromIntIndexedList) [(1,0)]       `shouldBe` [(1,0)]
    it "[(1,1)]"       $ (toDescList . fromIntIndexedList) [(1,1)]       `shouldBe` [(1,1)]
    it "[(2,0)]"       $ (toDescList . fromIntIndexedList) [(2,0)]       `shouldBe` [(2,0)]
    it "[(2,1)]"       $ (toDescList . fromIntIndexedList) [(2,1)]       `shouldBe` [(2,1)]
    it "[(2,2),(1,1)]" $ (toDescList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [(2,2),(1,1)]
    it "[(1,1),(2,2)]" $ (toDescList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [(2,2),(1,1)]
    it "[(5,2),(3,1)]" $ (toDescList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [(5,2),(3,1)]
    it "[(3,1),(5,2)]" $ (toDescList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [(5,2),(3,1)]

  describe "fromIndexedList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ (mapListToList . fromIntIndexedList) [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ (mapListToList . fromIntIndexedList) [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ (mapListToList . fromIntIndexedList) [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ (mapListToList . fromIntIndexedList) [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ (mapListToList . fromIntIndexedList) [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ (mapListToList . fromIntIndexedList) [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ (mapListToList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(1,1),(2,2)]" $ (mapListToList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ (mapListToList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]
    it "[(3,1),(5,2)]" $ (mapListToList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [0,0,0,1,0,2]

  describe "I.toList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ (I.toList . fromIntIndexedList) [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ (I.toList . fromIntIndexedList) [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ (I.toList . fromIntIndexedList) [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ (I.toList . fromIntIndexedList) [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ (I.toList . fromIntIndexedList) [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ (I.toList . fromIntIndexedList) [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ (I.toList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(1,1),(2,2)]" $ (I.toList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ (I.toList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]
    it "[(3,1),(5,2)]" $ (I.toList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [0,0,0,1,0,2]

  describe "LL.toList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ (LL.toList . fromIntIndexedList) [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ (LL.toList . fromIntIndexedList) [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ (LL.toList . fromIntIndexedList) [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ (LL.toList . fromIntIndexedList) [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ (LL.toList . fromIntIndexedList) [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ (LL.toList . fromIntIndexedList) [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ (LL.toList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(1,1),(2,2)]" $ (LL.toList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ (LL.toList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]
    it "[(3,1),(5,2)]" $ (LL.toList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [0,0,0,1,0,2]

listFromIntDescList :: [(Int , Int)] -> [Int]
listFromIntDescList = listFromDescList

fromIntIndexedList :: [(Int , Int)] -> MapList Int
fromIntIndexedList = fromIndexedList
