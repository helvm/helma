{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.WhiteCodelSliderSpec
  ( main
  , spec
  ) where

import           Data.Vector                                                   ( Vector )
import qualified Data.Vector                                                   as V
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.WhiteCodelSlider
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils
import           Test.Hspec
import           Text.InterpolatedString.Perl6

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "slideOnWhiteBlock" $ do
    forM_
      [ ("singleCodelImage (0, 0) rl", singleCodelImage, (0, 0), rl, ExitProgram)
      , ("oneLoopImage (1, 1) rl", oneLoopImage, (1, 1), rl, NextBlock NoOperation ur 2)
      , ("oneLoopImage (1, 1) rr", oneLoopImage, (1, 1), rr, NextBlock NoOperation ul 2)
      , ("gammaImage (1, 1) rl", gammaImage, (1, 1), rl, NextBlock NoOperation rl 1)
      , ("gammaImage (1, 4) rl", gammaImage, (1, 4), rl, NextBlock NoOperation ll 0)
      , ("crossShapedImage (2, 1) rl", crossShapedImage, (2, 1), rl, NextBlock NoOperation dr 5)
      , ("crossShapedImage (2, 1) rr", crossShapedImage, (2, 1), rr, NextBlock NoOperation dl 5)
      , ("crossShapedImage (2, 1) dl", crossShapedImage, (2, 1), dl, NextBlock NoOperation dl 5)
      , ("crossShapedImage (2, 1) dr", crossShapedImage, (2, 1), dr, NextBlock NoOperation dr 5)
      , ("crossShapedImage (2, 1) ll", crossShapedImage, (2, 1), ll, NextBlock NoOperation dr 5)
      , ("crossShapedImage (2, 1) lr", crossShapedImage, (2, 1), lr, NextBlock NoOperation dl 5)
      , ("crossShapedImage (2, 1) ul", crossShapedImage, (2, 1), ul, NextBlock NoOperation dl 5)
      , ("crossShapedImage (2, 1) ur", crossShapedImage, (2, 1), ur, NextBlock NoOperation dr 5)
      , ("crossShapedImage (1, 2) rl", crossShapedImage, (1, 2), rl, NextBlock NoOperation rl 5)
      , ("crossShapedImage (1, 2) rr", crossShapedImage, (1, 2), rr, NextBlock NoOperation rr 5)
      , ("crossShapedImage (1, 2) dl", crossShapedImage, (1, 2), dl, NextBlock NoOperation rr 5)
      , ("crossShapedImage (1, 2) dr", crossShapedImage, (1, 2), dr, NextBlock NoOperation rl 5)
      , ("crossShapedImage (1, 2) ll", crossShapedImage, (1, 2), ll, NextBlock NoOperation rl 5)
      , ("crossShapedImage (1, 2) lr", crossShapedImage, (1, 2), lr, NextBlock NoOperation rr 5)
      , ("crossShapedImage (1, 2) ul", crossShapedImage, (1, 2), ul, NextBlock NoOperation rr 5)
      , ("crossShapedImage (1, 2) ur", crossShapedImage, (1, 2), ur, NextBlock NoOperation rl 5)
      , ("crossShapedImage (3, 2) rl", crossShapedImage, (3, 2), rl, NextBlock NoOperation ll 5)
      , ("crossShapedImage (3, 2) rr", crossShapedImage, (3, 2), rr, NextBlock NoOperation lr 5)
      , ("crossShapedImage (3, 2) dl", crossShapedImage, (3, 2), dl, NextBlock NoOperation lr 5)
      , ("crossShapedImage (3, 2) dr", crossShapedImage, (3, 2), dr, NextBlock NoOperation ll 5)
      , ("crossShapedImage (3, 2) ll", crossShapedImage, (3, 2), ll, NextBlock NoOperation ll 5)
      , ("crossShapedImage (3, 2) lr", crossShapedImage, (3, 2), lr, NextBlock NoOperation lr 5)
      , ("crossShapedImage (3, 2) ul", crossShapedImage, (3, 2), ul, NextBlock NoOperation lr 5)
      , ("crossShapedImage (3, 2) ur", crossShapedImage, (3, 2), ur, NextBlock NoOperation ll 5)
      , ("crossShapedImage (2, 3) rl", crossShapedImage, (2, 3), rl, NextBlock NoOperation ur 5)
      , ("crossShapedImage (2, 3) rr", crossShapedImage, (2, 3), rr, NextBlock NoOperation ul 5)
      , ("crossShapedImage (2, 3) dl", crossShapedImage, (2, 3), dl, NextBlock NoOperation ul 5)
      , ("crossShapedImage (2, 3) dr", crossShapedImage, (2, 3), dr, NextBlock NoOperation ur 5)
      , ("crossShapedImage (2, 3) ll", crossShapedImage, (2, 3), ll, NextBlock NoOperation ur 5)
      , ("crossShapedImage (2, 3) lr", crossShapedImage, (2, 3), lr, NextBlock NoOperation ul 5)
      , ("crossShapedImage (2, 3) ul", crossShapedImage, (2, 3), ul, NextBlock NoOperation ul 5)
      , ("crossShapedImage (2, 3) ur", crossShapedImage, (2, 3), ur, NextBlock NoOperation ur 5)
      , ("spiralImage (1, 1) rl", spiralImage, (1, 1), rl, NextBlock NoOperation rl 4)
      , ("stuckImage1 (1, 1) rl", stuckImage1, (1, 1), rl, ExitProgram)
      , ("stuckImage2 (1, 1) rl", stuckImage2, (1, 1), rl, ExitProgram)
      , ("stuckImage3 (1, 1) rl", stuckImage3, (1, 1), rl, ExitProgram)
      ] $ \(name, image, initialPosition, initialDPCC, expected) ->
        context ("when given " ++ name) $
          it "slide and pure the next codel" $
            slideOnWhiteBlock image initialPosition initialDPCC `shouldBe` expected

singleCodelImage ∷ Vector (Vector (Codel, Int))
singleCodelImage = V.singleton $ V.singleton (WhiteCodel, 0)

oneLoopImage ∷ Vector (Vector (Codel, Int))
oneLoopImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y2         1
|] ∷ String))) where
  f '0' = (BlackCodel, 0)
  f 'r' = (AchromaticCodel Red Normal, 1)
  f 'g' = (AchromaticCodel Green Normal, 2)
  f 'b' = (AchromaticCodel Blue Normal, 3)
  f '1' = (BlackCodel, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '2' = (BlackCodel, 7)
  f _   = error "Unreachable"

gammaImage ∷ Vector (Vector (Codel, Int))
gammaImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
r          g
r       0  b
r          c
r          m
r     1    y
|] ∷ String))) where
  f 'r' = (AchromaticCodel Red Normal, 0)
  f 'g' = (AchromaticCodel Green Normal, 1)
  f 'b' = (AchromaticCodel Blue Normal, 2)
  f 'c' = (AchromaticCodel Cyan Normal, 3)
  f 'm' = (AchromaticCodel Magenta Normal, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '0' = (BlackCodel, 7)
  f '1' = (BlackCodel, 8)
  f _   = error "Unreachable"

crossShapedImage ∷ Vector (Vector (Codel, Int))
crossShapedImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
*****
**0**
*1r2*
**3**
*****
|] ∷ String))) where
  f '*' = (BlackCodel, 0)
  f '0' = (WhiteCodel, 1)
  f '1' = (WhiteCodel, 2)
  f '2' = (WhiteCodel, 3)
  f '3' = (WhiteCodel, 4)
  f 'r' = (AchromaticCodel Red Normal, 5)
  f _   = error "Unreachable"

spiralImage ∷ Vector (Vector (Codel, Int))
spiralImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
rrrrrrrrrrrg
y         0g
y 3        g
y       4  g
y   7      g
y     c    g
y  6       g
y      5   g
y2         g
y        1 g
ybbbbbbbbbbb
|] ∷ String))) where
  f 'r' = (AchromaticCodel Red Normal, 0)
  f 'g' = (AchromaticCodel Green Normal, 1)
  f 'b' = (AchromaticCodel Blue Normal, 2)
  f 'y' = (AchromaticCodel Yellow Normal, 3)
  f 'c' = (AchromaticCodel Cyan Normal, 4)
  f ' ' = (WhiteCodel, 5)
  f '0' = (BlackCodel, 6)
  f '1' = (BlackCodel, 7)
  f '2' = (BlackCodel, 8)
  f '3' = (BlackCodel, 9)
  f '4' = (BlackCodel, 10)
  f '5' = (BlackCodel, 11)
  f '6' = (BlackCodel, 12)
  f '7' = (BlackCodel, 13)
  f _   = error "Unreachable"

stuckImage1 ∷ Vector (Vector (Codel, Int))
stuckImage1 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y       2  1
|] ∷ String))) where
  f '0' = (BlackCodel, 0)
  f 'r' = (AchromaticCodel Red Normal, 1)
  f 'g' = (AchromaticCodel Green Normal, 2)
  f 'b' = (AchromaticCodel Blue Normal, 3)
  f '1' = (BlackCodel, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '2' = (BlackCodel, 7)
  f _   = error "Unreachable"

stuckImage2 ∷ Vector (Vector (Codel, Int))
stuckImage2 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y        2 1
|] ∷ String))) where
  f '0' = (BlackCodel, 0)
  f 'r' = (AchromaticCodel Red Normal, 1)
  f 'g' = (AchromaticCodel Green Normal, 2)
  f 'b' = (AchromaticCodel Blue Normal, 3)
  f '1' = (BlackCodel, 4)
  f 'y' = (AchromaticCodel Yellow Normal, 5)
  f ' ' = (WhiteCodel, 6)
  f '2' = (BlackCodel, 7)
  f _   = error "Unreachable"

stuckImage3 ∷ Vector (Vector (Codel, Int))
stuckImage3 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
00gb11111111
y          1
y          1
y          1
y          1
2          1
|] ∷ String))) where
  f '0' = (BlackCodel, 0)
  f 'g' = (AchromaticCodel Green Normal, 1)
  f 'b' = (AchromaticCodel Blue Normal, 2)
  f '1' = (BlackCodel, 3)
  f 'y' = (AchromaticCodel Yellow Normal, 4)
  f ' ' = (WhiteCodel, 5)
  f '2' = (BlackCodel, 6)
  f _   = error "Unreachable"
