module HelVM.HelMA.Automata.Piet.Parser
  ( parseColorImage
  , processImage
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Grid

import           HelVM.HelMA.Automata.Piet.API.CodelSize

import           Codec.Picture
import qualified Data.List.NonEmpty                          as NE
import           Data.MonoTraversable

processImage ∷ Maybe CodelSize → DynamicImage → (CodelSizeInternal, Grid Color)
processImage codelInfo dyn = (cs, imageToColorImage cs img) where
  (cs, img) = processJuicyImage codelInfo dyn

parseColorImage ∷ CodelSize → DynamicImage → Grid Color
parseColorImage nat dyn = imageToColorImage cs img where
  cs = fromIntegral nat
  img = convertRGB8 dyn

processJuicyImage ∷ Maybe CodelSize → DynamicImage → (CodelSizeInternal, Image PixelRGB8)
processJuicyImage codelInfo dynamicImage = (calculateActualCodelLength codelInfo img, img) where
  img = convertRGB8 dynamicImage

calculateActualCodelLength ∷ Maybe CodelSize → Image PixelRGB8 → Int
calculateActualCodelLength codelInfo img = max 1 $ maybe defaultCodelInfo fromIntegral codelInfo where
  defaultCodelInfo = imageGuessCodelLength img

imageGuessCodelLength ∷ Image PixelRGB8 → Int
imageGuessCodelLength img = fromMaybe 1 $ viaNonEmpty head (after <> before) where
  (before, after) = NE.break (== 1) values
  values = NE.scanr gcd (gcd w h) lengths
  lengths = gcd w h :| (fmap olength (group rows) <> fmap olength (group cols))

  rows = [ [ pixelAt img x y | x <- [0 .. w-1] ] | y <- [0 .. h-1] ]
  cols = [ [ pixelAt img x y | y <- [0 .. h-1] ] | x <- [0 .. w-1] ]

  w = imageWidth img
  h = imageHeight img

imageToColorImage ∷ Int → Image PixelRGB8 → Grid Color
imageToColorImage cs img = newGrid p (assocList cs img p) where
  p = (imageWidth img `div` cs, imageHeight img `div` cs)

assocList ∷ Int → Image PixelRGB8 → Coordinates → [(Coordinates, Color)]
assocList cs img (w, h) =
  [ ((x, y), pixelToColor (pixelAt img (x * cs) (y * cs)))
  | y <- [0 .. h - 1]
  , x <- [0 .. w - 1]
  ]
