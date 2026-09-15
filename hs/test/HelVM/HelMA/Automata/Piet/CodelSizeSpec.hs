{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.CodelSizeSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.CodelSize
import           HelVM.HelMA.Automata.Piet.TestUtils
import           HelVM.HelMA.Automata.Piet.Types.Grid

import qualified Data.Vector                          as V

import           Test.Hspec
import           Text.InterpolatedString.Perl6

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = describe "guessCodelSize" $ mapM_ runTest testCases where
  runTest (name, grid, codelSize) =
    context ("when given " ++ name) $
      it "pure the codel size of an image" $ guessCodelSize (width, height) imageF `shouldBe` codelSize
    where
      imageF (x, y) = image V.! y V.! x
      width = maybe 0 V.length (image V.!? 0)
      height = V.length image
      image = gridToMatrix grid

  testCases =
    [ ("smallestImage", smallestImage, 1)
    , ("size3Image", size3Image, 3)
    , ("size1Image", size1Image, 1)
    ]

smallestImage ∷ Grid Char
smallestImage = toGrid [['a']]

size3Image ∷ Grid Char
size3Image = toGrid $ toString <$> drop 1 (lines (toText ([q|
aaabbbbbb
aaabbbbbb
aaabbbbbb
ccccccddd
ccccccddd
ccccccddd
ccccccddd
ccccccddd
ccccccddd
|] ∷ String)))

size1Image ∷ Grid Char
size1Image = toGrid $ toString <$> drop 1 (lines (toText ([q|
aaabb
aaabb
aaabb
cccdd
cccdd
|] ∷ String)))
