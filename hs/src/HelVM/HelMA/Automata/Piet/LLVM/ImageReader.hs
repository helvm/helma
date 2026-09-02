{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module HelVM.HelMA.Automata.Piet.LLVM.ImageReader
  ( AdditionalColorStrategy (..)
  , CodelSize (..)
  , ImageConfig (..)
  , ImageReaderError (..)
  , Matrix
  , MulticoloredCodelStrategy (..)
  , imageToCodels
  , readCodels
  , rgbImageToCodels
  ) where

import           HelVM.HelMA.Automata.Piet.MatrixBuilder
import           HelVM.HelMA.Automata.Piet.ToRGB8

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Codec.Picture

import           Control.Monad.Except                        ( MonadError (throwError), liftEither )

data ImageReaderError
  = ReadImageFileError String
  | UnsupportedImageError String
  | CodelSizeError
  deriving stock (Eq, Show)

type MonadImageError m = MonadError ImageReaderError m

readCodels ∷ (MonadIO m, MonadImageError m) ⇒ ImageConfig → FilePath → m (Matrix Color)
readCodels config = (=<<) (imageToCodels config) . (=<<) (liftEither . first ReadImageFileError) . liftIO . readImage

imageToCodels ∷ MonadImageError m ⇒ ImageConfig → DynamicImage → m (Matrix Color)
imageToCodels config = (=<<) (rgbImageToCodels config) . liftEither . first UnsupportedImageError . toRGB8ImageM

rgbImageToCodels ∷ MonadImageError m ⇒ ImageConfig → Image PixelRGB8 → m (Matrix Color)
rgbImageToCodels config image = checkDimensions (modX, modY) $> buildMatrix (codelWidth, codelHeight) config codelSizeInt image where
  (codelWidth, modX) = divMod pixelWidth codelSizeInt
  (codelHeight, modY) = divMod pixelHeight codelSizeInt
  codelSizeInt = getIntCodelSize (pixelWidth, pixelHeight) image (codelSize config)
  pixelWidth = imageWidth image
  pixelHeight = imageHeight image

checkDimensions ∷ MonadImageError m ⇒ Coordinates → m ()
checkDimensions (0, 0) = pass
checkDimensions _      = throwError CodelSizeError
