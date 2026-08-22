module HelVM.HelMA.Automata.Piet.ColorMapParser
  ( imageToColorMap
  , parseColorMap
  , parseColorMapSafe
  , pixelToColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.ColorMap
import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

import qualified Data.Vector                                   as Vector

import qualified Relude.Extra                                  as Extra

parseColorMapSafe ∷ MonadSafe f ⇒ CodelSize → DynamicImage → f ColorMap
parseColorMapSafe cs dyn = imageToColorMap cs <$> toRGB8OSafe dyn

parseColorMap ∷ CodelSize → DynamicImage → EitherError ColorMap
parseColorMap cs dyn = imageToColorMap cs <$> toRGB8OrError dyn

imageToColorMap ∷ CodelSize → Image PixelRGB8 → ColorMap
imageToColorMap cs img = ColorMap matrix' w' (Vector.length matrix') where
  matrix' = to2D w' . Vector.fromList $ map (pixelToColor . pixAt) coords
  coords = [(x, y) | y <- [0..Extra.prev h], x <- [0..Extra.prev w], cs |^ x, cs |^ y]
  w = imageWidth img
  h = imageHeight img
  w' = w `div` cs
  a |^ b = b `mod` a == 0
  pixAt = uncurry (pixelAt img)

to2D ∷ Int → Vector.Vector a → Vector.Vector (Vector.Vector a)
to2D width v
  | Vector.null v = Vector.empty
  | otherwise = uncurry collect $ Vector.splitAt width v
  where
    collect h t
      | Vector.length t < width = Vector.singleton h
      | otherwise          = Vector.cons h (to2D width t)
