module HelVM.HelMA.Automata.Piet.ColorMapParser
  ( imageToColorImage
  , parseColorImage
  , parseColorImageSafe
  , pixelToColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image        
import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig

import           HelVM.HelIO.Control.Safe

import qualified Codec.Picture                                 as Picture

import qualified Relude.Extra                                  as Extra

parseColorImageSafe :: MonadSafe f => CodelSize -> Picture.DynamicImage -> f (Image Color)
parseColorImageSafe cs dyn = imageToColorImage cs <$> toRGB8OSafe dyn

parseColorImage :: CodelSize -> Picture.DynamicImage -> EitherError (Image Color)
parseColorImage cs dyn = imageToColorImage cs <$> toRGB8OrError dyn

imageToColorImage :: CodelSize -> Picture.Image Picture.PixelRGB8 -> Image Color
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
