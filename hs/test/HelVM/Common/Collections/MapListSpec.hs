module HelVM.Common.Collections.MapListSpec (spec)  where

import           HelVM.Common.Collections.MapList

import qualified Data.ListLike                    as LL
import qualified GHC.Exts                         as I (IsList (..))

import           Test.Hspec                       (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "fromList" $ do
    it "[0]"       $ do I.toList (fromList [0]       :: MapList Int) `shouldBe` [0]
    it "[1]"       $ do I.toList (fromList [1]       :: MapList Int) `shouldBe` [1]
    it "[0,1]"     $ do I.toList (fromList [0,1]     :: MapList Int) `shouldBe` [0,1]
    it "[1,0]"     $ do I.toList (fromList [1,0]     :: MapList Int) `shouldBe` [1,0]
    it "[0,1,2,3]" $ do I.toList (fromList [0,1,2,3] :: MapList Int) `shouldBe` [0,1,2,3]
    it "[3,2,1,0]" $ do I.toList (fromList [3,2,1,0] :: MapList Int) `shouldBe` [3,2,1,0]

  describe "listFromIntDescList" $ do
    it "[(0,0)]"       $ do listFromIntDescList [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ do listFromIntDescList [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ do listFromIntDescList [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ do listFromIntDescList [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ do listFromIntDescList [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ do listFromIntDescList [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ do listFromIntDescList [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ do listFromIntDescList [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]

  describe "toDescList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ do (toDescList . fromIntIndexedList) [(0,0)]       `shouldBe` [(0,0)]
    it "[(0,1)]"       $ do (toDescList . fromIntIndexedList) [(0,1)]       `shouldBe` [(0,1)]
    it "[(1,0)]"       $ do (toDescList . fromIntIndexedList) [(1,0)]       `shouldBe` [(1,0)]
    it "[(1,1)]"       $ do (toDescList . fromIntIndexedList) [(1,1)]       `shouldBe` [(1,1)]
    it "[(2,0)]"       $ do (toDescList . fromIntIndexedList) [(2,0)]       `shouldBe` [(2,0)]
    it "[(2,1)]"       $ do (toDescList . fromIntIndexedList) [(2,1)]       `shouldBe` [(2,1)]
    it "[(2,2),(1,1)]" $ do (toDescList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [(2,2),(1,1)]
    it "[(1,1),(2,2)]" $ do (toDescList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [(2,2),(1,1)]
    it "[(5,2),(3,1)]" $ do (toDescList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [(5,2),(3,1)]
    it "[(3,1),(5,2)]" $ do (toDescList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [(5,2),(3,1)]

  describe "fromIndexedList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ do (mapListToList . fromIntIndexedList) [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ do (mapListToList . fromIntIndexedList) [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ do (mapListToList . fromIntIndexedList) [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ do (mapListToList . fromIntIndexedList) [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ do (mapListToList . fromIntIndexedList) [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ do (mapListToList . fromIntIndexedList) [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ do (mapListToList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(1,1),(2,2)]" $ do (mapListToList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ do (mapListToList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]
    it "[(3,1),(5,2)]" $ do (mapListToList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [0,0,0,1,0,2]

  describe "I.toList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ do (I.toList . fromIntIndexedList) [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ do (I.toList . fromIntIndexedList) [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ do (I.toList . fromIntIndexedList) [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ do (I.toList . fromIntIndexedList) [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ do (I.toList . fromIntIndexedList) [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ do (I.toList . fromIntIndexedList) [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ do (I.toList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(1,1),(2,2)]" $ do (I.toList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ do (I.toList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]
    it "[(3,1),(5,2)]" $ do (I.toList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [0,0,0,1,0,2]

  describe "LL.toList . fromIntIndexedList" $ do
    it "[(0,0)]"       $ do (LL.toList . fromIntIndexedList) [(0,0)]       `shouldBe` [0]
    it "[(0,1)]"       $ do (LL.toList . fromIntIndexedList) [(0,1)]       `shouldBe` [1]
    it "[(1,0)]"       $ do (LL.toList . fromIntIndexedList) [(1,0)]       `shouldBe` [0,0]
    it "[(1,1)]"       $ do (LL.toList . fromIntIndexedList) [(1,1)]       `shouldBe` [0,1]
    it "[(2,0)]"       $ do (LL.toList . fromIntIndexedList) [(2,0)]       `shouldBe` [0,0,0]
    it "[(2,1)]"       $ do (LL.toList . fromIntIndexedList) [(2,1)]       `shouldBe` [0,0,1]
    it "[(2,2),(1,1)]" $ do (LL.toList . fromIntIndexedList) [(2,2),(1,1)] `shouldBe` [0,1,2]
    it "[(1,1),(2,2)]" $ do (LL.toList . fromIntIndexedList) [(1,1),(2,2)] `shouldBe` [0,1,2]
    it "[(5,2),(3,1)]" $ do (LL.toList . fromIntIndexedList) [(5,2),(3,1)] `shouldBe` [0,0,0,1,0,2]
    it "[(3,1),(5,2)]" $ do (LL.toList . fromIntIndexedList) [(3,1),(5,2)] `shouldBe` [0,0,0,1,0,2]

listFromIntDescList :: [(Int , Int)] -> [Int]
listFromIntDescList = listFromDescList

fromIntIndexedList :: [(Int , Int)] -> MapList Int
fromIntIndexedList = fromIndexedList
