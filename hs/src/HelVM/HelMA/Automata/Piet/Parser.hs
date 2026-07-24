module HelVM.HelMA.Automata.Piet.Parser
  ( processImage
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image

import           HelVM.HelMA.Automaton.Eff.MonadEff

import qualified Codec.Picture                         as Picture

import qualified Data.ListLike                         as LL

processImage :: AppEff m => Maybe Int -> Picture.DynamicImage -> m (Image Color)
processImage codelInfo dynamicImage = buildImage =<< determineCodelLength codelInfo img where
  img = Picture.convertRGBA8 dynamicImage
  buildImage actualCodelLength = pure $ imageFromJuicy actualCodelLength img

determineCodelLength :: AppEff m => Maybe Int -> Picture.Image Picture.PixelRGBA8 -> m Int
determineCodelLength codelInfo img = limitCodel =<< maybe (imageGuessCodelLength img) pure codelInfo where
  limitCodel = pure . max 1

imageFromJuicy :: Int -> Picture.Image Picture.PixelRGBA8 -> Image Color
imageFromJuicy codelLength img = newImage (width, height) pixels where
  width  = Picture.imageWidth img `div` codelLength
  height = Picture.imageHeight img `div` codelLength

  pixels = [ ((x, y), extractColor x y) | x <- [0 .. width-1], y <- [0 .. height-1] ]

  extractColor x y = checkAlpha (Picture.pixelAt img (x * codelLength) (y * codelLength))
  checkAlpha (Picture.PixelRGBA8 r g b _) = rgb2Color r g b

imageGuessCodelLength :: AppEff m => Picture.Image Picture.PixelRGBA8 -> m Int
imageGuessCodelLength img = pure $ lastUntil isOne $ scanl gcd (gcd width height) $ fmap LL.length (group rows) ++ fmap LL.length (group cols) where
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
