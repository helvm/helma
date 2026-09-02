{-# LANGUAGE FlexibleContexts #-}

module HelVM.HelMA.Automata.Piet.ToRGB8
  ( ToRGB8 (..)
  , toRGB8ImageM
  ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad.Except
import           Data.Bits

import qualified Relude.Extra         as Extra

toRGB8ImageM ∷ MonadError String m ⇒ DynamicImage → m (Image PixelRGB8)
toRGB8ImageM (ImageY8     _)     = failConversion
toRGB8ImageM (ImageY16    _)     = failConversion
toRGB8ImageM (ImageY32    _)     = failConversion
toRGB8ImageM (ImageYF     _)     = failConversion
toRGB8ImageM (ImageYA8    _)     = failConversion
toRGB8ImageM (ImageYA16   _)     = failConversion
toRGB8ImageM (ImageRGB8   image) = pure $ toRGB8Image image
toRGB8ImageM (ImageRGB16  image) = pure $ toRGB8Image image
toRGB8ImageM (ImageRGBF   image) = pure $ toRGB8Image image
toRGB8ImageM (ImageRGBA8  image) = pure $ toRGB8Image image
toRGB8ImageM (ImageRGBA16 image) = pure $ toRGB8Image image
toRGB8ImageM (ImageYCbCr8 image) = pure $ toRGB8Image image
toRGB8ImageM (ImageCMYK8  image) = pure $ toRGB8Image image
toRGB8ImageM (ImageCMYK16 image) = pure $ toRGB8Image image

failConversion ∷ MonadError String m ⇒ m a
failConversion = throwError "can't convert from grayscale images"

class Pixel a => ToRGB8 a where
  toRGB8Pixel :: a → PixelRGB8
  toRGB8Image :: Image a → Image PixelRGB8
  toRGB8Image = pixelMap toRGB8Pixel

instance ToRGB8 PixelRGB8 where
  toRGB8Pixel = id

instance ToRGB8 PixelRGB16 where
  toRGB8Pixel (PixelRGB16 r g b) = PixelRGB8 (drop8 r) (drop8 g) (drop8 b) where
    drop8 x = fromMaybe 0 . Extra.safeToEnum . fromEnum $ shiftR x 8

instance ToRGB8 PixelRGBF where
  toRGB8Pixel (PixelRGBF r g b) = PixelRGB8 (toI r) (toI g) (toI b) where
    toI = floor . (* 255) . max 0 . min 1

instance ToRGB8 PixelRGBA8 where
  toRGB8Pixel = dropTransparency

instance ToRGB8 PixelRGBA16 where
  toRGB8Pixel = toRGB8Pixel . dropTransparency

instance ToRGB8 PixelCMYK8 where
  toRGB8Pixel = convertPixel

instance ToRGB8 PixelCMYK16 where
  toRGB8Pixel = toRGB8Pixel . (convertPixel :: PixelCMYK16 → PixelRGB16)

instance ToRGB8 PixelYCbCr8 where
  toRGB8Pixel = convertPixel
