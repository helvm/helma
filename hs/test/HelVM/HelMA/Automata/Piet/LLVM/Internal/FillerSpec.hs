{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.LLVM.Internal.FillerSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Filler
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import qualified Data.IntMap                                    as IM
import qualified Data.Set                                       as S
import           Data.Vector                                    ( Vector )
import qualified Data.Vector                                    as V

import           Test.Hspec
import           Text.InterpolatedString.Perl6

data TestCase
  = TestCase
      { caseName            :: String
      , inputImage          :: Vector (Vector Char)
      , expectedFilledImage :: Vector (Vector Int)
      , expectedCoords      :: [(Int, Set Coordinates)]
      }

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = describe "fillAll" $ forM_ testCases runTest where
  runTest tc = context ("when given " ++ caseName tc) $ do
    it "fills all blocks in a given image" $ filledImage `shouldBe` expectedFilledImage tc
    it "returns coordinates of filled blocks" $ fmap S.fromList <$> IM.toAscList coords `shouldBe` expectedCoords tc
    where
      (filledImage, coords) = fillAll (inputImage tc)

  testCases =
    [ TestCase "emptyImage" V.empty V.empty []
    , TestCase "smallImage" smallImage expectedFilledSmallImage expectedSmallCoords
    , TestCase "complexImage" complexImage expectedFilledComplexImage expectedComplexCoords
    , TestCase "irregularImage" irregularImage expectedFilledIrregularImage expectedIrregularCoords
    ]

smallImage ∷ Vector (Vector Char)
smallImage = toVector2D [['a']]

expectedFilledSmallImage ∷ Vector (Vector Int)
expectedFilledSmallImage = toVector2D [[0]]

expectedSmallCoords ∷ [(Int, Set Coordinates)]
expectedSmallCoords = [(0, S.fromList [(0, 0)])]

complexImage ∷ Vector (Vector Char)
complexImage = toVector2D $ toString <$> drop 1 (lines (toText ([q|
GGGGGBrrr rrrMMM
bbbBBBBB    YYY*
bbbbRBBR  YYY**m
cccRRRRRR*****mm
  cccRRRRRRR*mm*
   cccccRg**mmm*
        grrr*GGr
 y    CC ggg   *
|] ∷ String)))

expectedFilledComplexImage ∷ Vector (Vector Int)
expectedFilledComplexImage = charToOrd <<$>> toVector2D (toString <$> drop 1 (lines (toText ([q|
aaaaabcccdeeefff
gggbbbbbddddhhhi
ggggjbbjddhhhkkl
mmmjjjjjjkkkkkll
nnmmmjjjjjjjkllo
nnnmmmmmjpqqlllo
nnnnnnnnrssstuuv
nwnnnnxxyzzz{{{|
|] ∷ String))))

expectedComplexCoords ∷ [(Int, Set Coordinates)]
expectedComplexCoords =
  [ (0, S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)])
  , (1, S.fromList [(5, 0), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (5, 2), (6, 2)])
  , (2, S.fromList [(6, 0), (7, 0), (8, 0)])
  , (3, S.fromList [(9, 0), (8, 1), (9, 1), (10, 1), (11, 1), (8, 2), (9, 2)])
  , (4, S.fromList [(10, 0), (11, 0), (12, 0)])
  , (5, S.fromList [(13, 0), (14, 0), (15, 0)])
  , (6, S.fromList [(0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2), (3, 2)])
  , (7, S.fromList [(12, 1), (13, 1), (14, 1), (10, 2), (11, 2), (12, 2)])
  , (8, S.fromList [(15, 1)])
  , (9, S.fromList [(4, 2), (7, 2), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (5, 4), (6, 4), (7, 4), (8, 4), (9, 4), (10, 4), (11, 4), (8, 5)])
  , (10, S.fromList [(13, 2), (14, 2), (9, 3), (10, 3), (11, 3), (12, 3), (13, 3), (12, 4)])
  , (11, S.fromList [(15, 2), (14, 3), (15, 3), (13, 4), (14, 4), (12, 5), (13, 5), (14, 5)])
  , (12, S.fromList [(0, 3), (1, 3), (2, 3), (2, 4), (3, 4), (4, 4), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5)])
  , (13, S.fromList [(0, 4), (1, 4), (0, 5), (1, 5), (2, 5), (0, 6), (1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (0, 7), (2, 7), (3, 7), (4, 7), (5, 7)])
  , (14, S.fromList [(15, 4), (15, 5)])
  , (15, S.fromList [(9, 5)])
  , (16, S.fromList [(10, 5), (11, 5)])
  , (17, S.fromList [(8, 6)])
  , (18, S.fromList [(9, 6), (10, 6), (11, 6)])
  , (19, S.fromList [(12, 6)])
  , (20, S.fromList [(13, 6), (14, 6)])
  , (21, S.fromList [(15, 6)])
  , (22, S.fromList [(1, 7)])
  , (23, S.fromList [(6, 7), (7, 7)])
  , (24, S.fromList [(8, 7)])
  , (25, S.fromList [(9, 7), (10, 7), (11, 7)])
  , (26, S.fromList [(12, 7), (13, 7), (14, 7)])
  , (27, S.fromList [(15, 7)])
  ]

irregularImage ∷ Vector (Vector Char)
irregularImage = toVector2D $ toString <$> drop 1 (lines (toText ([q|
abaa
cca
c
cccaaaa
|] ∷ String)))

expectedFilledIrregularImage ∷ Vector (Vector Int)
expectedFilledIrregularImage = charToOrd <<$>> toVector2D (toString <$> drop 1 (lines (toText ([q|
abcc
ddc
d
dddeeee
|] ∷ String))))

expectedIrregularCoords ∷ [(Int, Set Coordinates)]
expectedIrregularCoords =
  [ (0, S.fromList [(0, 0)])
  , (1, S.fromList [(1, 0)])
  , (2, S.fromList [(2, 0), (3, 0), (2, 1)])
  , (3, S.fromList [(0, 1), (1, 1), (0, 2), (0, 3), (1, 3), (2, 3)])
  , (4, S.fromList [(3, 3), (4, 3), (5, 3), (6, 3)])
  ]

charToOrd ∷ Char → Int
charToOrd c = ord c - ord 'a'
