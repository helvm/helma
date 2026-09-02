module HelVM.HelMA.Automata.Piet.ToRGB8Spec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.ToRGB8

import           Codec.Picture
import           Test.Hspec

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "toRGB8ImageM" $ do
    forM_
      [ ("ImageY8", ImageY8 $ singlePixelImage 0x7F)
      , ("ImageY16", ImageY16 $ singlePixelImage 0x7FFF)
      , ("ImageYF", ImageYF $ singlePixelImage 0.5)
      , ("ImageYA8", ImageYA8 $ singlePixelImage $ PixelYA8 0x7F 0xFF)
      , ("ImageYA1", ImageYA16 $ singlePixelImage $ PixelYA16 0x7FFF 0xFFFF)
      ] $ \(name, image) ->
        context ("when given " ++ name) $ do
          it "fails" $ isLeft (toRGB8ImageM image) `shouldBe` True
    forM_
      [ ("ImageRGB8", ImageRGB8 $ singlePixelImage $ PixelRGB8 0x00 0x7F 0xFF)
      , ("ImageRGB16", ImageRGB16 $ singlePixelImage $ PixelRGB16 0x0000 0x7FFF 0xFFFF)
      , ("ImageRGBF", ImageRGBF $ singlePixelImage $ PixelRGBF 0.0 0.5 1.0)
      , ("ImageRGBA8", ImageRGBA8 $ singlePixelImage $ PixelRGBA8 0x00 0x7F 0xFF 0xFF)
      , ("ImageRGBA16", ImageRGBA16 $ singlePixelImage $ PixelRGBA16 0x0000 0x7FFF 0xFFFF 0xFFFF)
      , ("ImageYCbCr8", ImageYCbCr8 $ singlePixelImage $ PixelYCbCr8 104 213 54)
      , ("ImageCMYK8", ImageCMYK8 $ singlePixelImage $ PixelCMYK8 0xFF 0x7F 0 0)
      , ("ImageCMYK16", ImageCMYK16 $ singlePixelImage $ PixelCMYK16 0xFFFF 0x7FFF 0 0)
      ] $ \(name, image) ->
        context ("when given " ++ name) $ do
          let expectedImage127 = imageData $ singlePixelImage $ PixelRGB8 0x00 0x7F 0xFF
          let expectedImage128 = imageData $ singlePixelImage $ PixelRGB8 0x00 0x80 0xFF
          let actualImage = imageData $ fromRight (error "Unreachable") $ toRGB8ImageM image
          it "returns a RGB8 image" $ actualImage == expectedImage127 || actualImage == expectedImage128 `shouldBe` True

singlePixelImage ∷ Pixel a ⇒ a → Image a
singlePixelImage pixel = generateImage (\_ _ -> pixel) 1 1
