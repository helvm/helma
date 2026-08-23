module HelVM.HelMA.Automata.Piet.Parser
  ( imageToColorImage
  , parseColorImage
  , pixelToColor
  , processImage
  , processImageWithLog
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig

import           HelVM.HelIO.Control.Safe

import qualified Codec.Picture                                 as Picture

import           Control.Monad.Logger

import           Data.MonoTraversable

parseColorImage :: MonadSafe f => CodelSize -> Picture.DynamicImage -> f (Image Color)
parseColorImage cs dyn = imageToColorImage cs pixelToColor <$> toRGB8 dyn

-- Generic image converter with step/codel sampling
imageToColorImage :: Picture.Pixel pixel => CodelSize -> (pixel -> Color) -> Picture.Image pixel -> Image Color
imageToColorImage cs convertPixel img = newImage (w', h') assocList
  where
    w  = Picture.imageWidth img
    h  = Picture.imageHeight img
    w' = w `div` cs
    h' = h `div` cs

    assocList =
      [ ((x, y), convertPixel (Picture.pixelAt img (x * cs) (y * cs)))
      | y <- [0 .. h' - 1]
      , x <- [0 .. w' - 1]
      ]

processImageWithLog :: MonadLogger m => Maybe Natural -> Picture.DynamicImage -> m (Image Color)
processImageWithLog codelInfo dyn = do
  let (cs, img) = processJuicyImage codelInfo dyn
  logDebugN ("Actual codel length: " <> show cs)
  pure $ imageToColorImage cs rgba2Color img

processImage :: Maybe Natural -> Picture.DynamicImage -> Image Color
processImage codelInfo dyn = imageToColorImage cs rgba2Color img
  where
    (cs, img) = processJuicyImage codelInfo dyn

-- Helper to convert PixelRGBA8 directly to Color
rgba2Color :: Picture.PixelRGBA8 -> Color
rgba2Color (Picture.PixelRGBA8 r g b _) = rgb2Color r g b

-- Common pipeline for resolving codel length and converting dynamic image
processJuicyImage :: Maybe Natural -> Picture.DynamicImage -> (CodelSize, Picture.Image Picture.PixelRGBA8)
processJuicyImage codelInfo dynamicImage = (actualCodelLength, img)
  where
    img = Picture.convertRGBA8 dynamicImage
    actualCodelLength = calculateActualCodelLength codelInfo img

calculateActualCodelLength :: Maybe Natural -> Picture.Image Picture.PixelRGBA8 -> Int
calculateActualCodelLength codelInfo img = max 1 $ maybe defaultCodelInfo fromIntegral codelInfo
  where
    defaultCodelInfo = imageGuessCodelLength img

imageGuessCodelLength :: Picture.Image Picture.PixelRGBA8 -> Int
imageGuessCodelLength img = lastUntil isOne $ scanl gcd (gcd width height) $ fmap olength (group rows) <> fmap olength (group cols)
  where
    width  = Picture.imageWidth img
    height = Picture.imageHeight img
    isOne  = (== 1)

    rows = [ [ Picture.pixelAt img x y | x <- [0 .. width-1] ]  | y <- [0 .. height-1] ]
    cols = [ [ Picture.pixelAt img x y | y <- [0 .. height-1] ] | x <- [0 .. width-1] ]

    lastUntil _ [x]    = x
    lastUntil p (x:xs) = guardPred (p x) p x xs
    lastUntil _ _      = error "empty list in lastUntil helper (imageGuessCodelLength)"

    guardPred True  _  x _  = x
    guardPred False p' _ xs = lastUntil p' xs
    