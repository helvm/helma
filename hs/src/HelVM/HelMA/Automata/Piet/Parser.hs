module HelVM.HelMA.Automata.Piet.Parser
  ( parseColorImage
  , processImage
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig

import qualified Codec.Picture                                 as Picture

import           Control.Monad.Logger

import           Data.MonoTraversable

processImage ∷ MonadLogger m ⇒ Maybe Natural → Picture.DynamicImage → m (Image Color)
processImage codelInfo dyn = do
  let (cs, img) = processJuicyImage codelInfo dyn
  logDebugN ("Actual codel length: " <> show cs)
  pure $ imageToColorImage cs img

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

imageToColorImage ∷ Int → Picture.Image Picture.PixelRGB8 → Image Color
imageToColorImage cs img = newImage (w', h') assocList
  where
    w  = Picture.imageWidth img
    h  = Picture.imageHeight img
    w' = w `div` cs
    h' = h `div` cs

    assocList =
      [ ((x, y), pixelToColor (Picture.pixelAt img (x * cs) (y * cs)))
      | y <- [0 .. h' - 1]
      , x <- [0 .. w' - 1]
      ]
