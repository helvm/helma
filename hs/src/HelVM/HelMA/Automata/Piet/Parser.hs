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

import qualified Relude.Extra                                  as Extra

parseColorImage ∷ MonadSafe f ⇒ CodelSize → Picture.DynamicImage → f (Image Color)
parseColorImage cs dyn = imageToColorImage cs <$> toRGB8 dyn

imageToColorImage ∷ CodelSize → Picture.Image Picture.PixelRGB8 → Image Color
imageToColorImage cs img = newImage (w', h') assocList where
  w = Picture.imageWidth img
  h = Picture.imageHeight img
  w' = w `div` cs
  h' = h `div` cs

  assocList =
    [ ((x `div` cs, y `div` cs), pixelToColor (Picture.pixelAt img x y))
    | y <- [0..Extra.prev h]
    , x <- [0..Extra.prev w]
    , cs |^ x
    , cs |^ y
    ]

  a |^ b = b `mod` a == 0

processImageWithLog ∷ MonadLogger m ⇒ Maybe Natural → Picture.DynamicImage → m (Image Color)
processImageWithLog codelInfo dynamicImage = imageFromJuicy actualCodelLength img <$ logDebugN ("Actual codel length: " <> show actualCodelLength) where
  actualCodelLength = calculateActualCodelLength codelInfo img
  img = Picture.convertRGBA8 dynamicImage

processImage ∷  Maybe Natural → Picture.DynamicImage → Image Color
processImage codelInfo dynamicImage = imageFromJuicy actualCodelLength img where
  actualCodelLength = calculateActualCodelLength codelInfo img
  img = Picture.convertRGBA8 dynamicImage

calculateActualCodelLength ∷ Maybe Natural → Picture.Image Picture.PixelRGBA8 → Int
calculateActualCodelLength codelInfo img  = max 1 $ maybe defaultCodelInfo fromIntegral codelInfo where
    defaultCodelInfo = imageGuessCodelLength img

imageFromJuicy ∷ Int → Picture.Image Picture.PixelRGBA8 → Image Color
imageFromJuicy codelLength img = newImage (width, height) pixels where
  width  = Picture.imageWidth img `div` codelLength
  height = Picture.imageHeight img `div` codelLength

  pixels = [ ((x, y), extractColor x y) | x <- [0 .. width-1], y <- [0 .. height-1] ]

  extractColor x y = checkAlpha (Picture.pixelAt img (x * codelLength) (y * codelLength))
  checkAlpha (Picture.PixelRGBA8 r g b _) = rgb2Color r g b

imageGuessCodelLength ∷ Picture.Image Picture.PixelRGBA8 → Int
imageGuessCodelLength img = lastUntil isOne $ scanl gcd (gcd width height) $ fmap olength (group rows) <> fmap olength (group cols) where
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
