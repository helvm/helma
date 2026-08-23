module HelVM.HelMA.Automata.Piet.Parser
  ( parseColorImage
  , processImage
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig

import qualified Codec.Picture                                 as Picture

import           Control.Monad.Logger

import           Data.MonoTraversable

processImage ∷ MonadLogger m ⇒ Maybe Natural → Picture.DynamicImage → m (CodelSize, Image Color)
processImage codelInfo dyn = (cs, imageToColorImage cs img) <$ logDebugN ("Actual codel length: " <> show cs) where
  (cs, img) = processJuicyImage codelInfo dyn

parseColorImage ∷ Natural → Picture.DynamicImage → Image Color
parseColorImage nat dyn = imageToColorImage cs img where
  cs = fromIntegral nat
  img = Picture.convertRGB8 dyn

processJuicyImage ∷ Maybe Natural → Picture.DynamicImage → (CodelSize, Picture.Image Picture.PixelRGB8)
processJuicyImage codelInfo dynamicImage = (calculateActualCodelLength codelInfo img, img) where
  img = Picture.convertRGB8 dynamicImage

calculateActualCodelLength ∷ Maybe Natural → Picture.Image Picture.PixelRGB8 → Int
calculateActualCodelLength codelInfo img = max 1 $ maybe defaultCodelInfo fromIntegral codelInfo where
  defaultCodelInfo = imageGuessCodelLength img

imageGuessCodelLength ∷ Picture.Image Picture.PixelRGB8 → Int
imageGuessCodelLength img = fromMaybe 1 $ viaNonEmpty head (after <> before) where
  (before, after) = break (== 1) (reverse values)
  values = scanl gcd (gcd w h) $ fmap olength (group rows) <> fmap olength (group cols)

  rows = [ [ Picture.pixelAt img x y | x <- [0 .. w-1] ] | y <- [0 .. h-1] ]
  cols = [ [ Picture.pixelAt img x y | y <- [0 .. h-1] ] | x <- [0 .. w-1] ]

  w  = Picture.imageWidth img
  h = Picture.imageHeight img

imageToColorImage ∷ Int → Picture.Image Picture.PixelRGB8 → Image Color
imageToColorImage cs img = newImage p (assocList cs img p) where
  p = (Picture.imageWidth img `div` cs, Picture.imageHeight img `div` cs)

assocList ∷ Int → Picture.Image Picture.PixelRGB8 → Coordinates → [(Coordinates, Color)]
assocList cs img (w, h) =
  [ ((x, y), pixelToColor (Picture.pixelAt img (x * cs) (y * cs)))
  | y <- [0 .. h - 1]
  , x <- [0 .. w - 1]
  ]
