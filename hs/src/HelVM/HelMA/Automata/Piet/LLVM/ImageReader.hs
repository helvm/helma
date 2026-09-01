{-# LANGUAGE FlexibleContexts #-}

module HelVM.HelMA.Automata.Piet.LLVM.ImageReader
  ( AdditionalColorStrategy (..)
  , CodelSize (..)
  , ImageConfig (..)
  , ImageReaderError (..)
  , MulticoloredCodelStrategy (..)
  , imageToCodels
  , readCodels
  , rgbImageToCodels
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.CodelSize
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.ToRGB8

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Codec.Picture

import           Control.Monad.Except

import qualified Data.Foldable1                                    as F1
import qualified Data.List.NonEmpty                                as NE
import qualified Data.Map                                          as M
import           Data.Vector                                       ( Vector )
import qualified Data.Vector                                       as V

data ImageReaderError
  = ReadImageFileError String -- ^ The image file is unreadable.
  | UnsupportedImageError String -- ^ The input image has an unsupported format.
  | CodelSizeError -- ^ The specified size of codel is not fit for the image.
  deriving stock (Eq, Show)

data AdditionalColorStrategy
  = AdditionalColorAsWhite -- ^ Treating as a white codel.
  | AdditionalColorAsBlack -- ^ Treating as a black codel.
  | AdditionalColorNearest -- ^ Treating as a codel which has the nearest color.
  deriving stock (Eq, Ord, Show)

data MulticoloredCodelStrategy
  = MulticoloredCodelAsWhite -- ^ Treating as a white codel.
  | MulticoloredCodelAsBlack -- ^ Treating as a black codel.
  | MulticoloredCodelCenter -- ^ Picking up a center pixel.
  | MulticoloredCodelModal -- ^ Finding the modal color, the most frequent color.
  | MulticoloredCodelAverage -- ^ Calculating an average color.
  deriving stock (Eq, Ord, Show)

data CodelSize
  = CodelSize Int
  | GuessCodelSize
  deriving stock (Eq, Show)

data ImageConfig
  = ImageConfig
      { additionalColor   :: AdditionalColorStrategy
      , multicoloredCodel :: MulticoloredCodelStrategy
      , codelSize         :: CodelSize
      }
  deriving stock (Eq, Show)

readCodels ∷ (MonadIO m, MonadError ImageReaderError m) ⇒ ImageConfig → FilePath → m (Vector (Vector Codel))
readCodels config path = do
  imageEither <- liftIO $ readImage path
  image <- liftEither $ first ReadImageFileError imageEither
  imageToCodels config image

imageToCodels ∷ MonadError ImageReaderError m ⇒ ImageConfig → DynamicImage → m (Vector (Vector Codel))
imageToCodels config dynamicImage = do
  rgbImage <- liftEither $ first UnsupportedImageError $ toRGB8ImageM dynamicImage
  rgbImageToCodels config rgbImage

rgbImageToCodels ∷ MonadError ImageReaderError m ⇒ ImageConfig → Image PixelRGB8 → m (Vector (Vector Codel))
rgbImageToCodels config image = do
  when (modX /= 0 || modY /= 0) $ throwError CodelSizeError
  pure $ V.generate codelHeight $ \codelY ->
    V.generate codelWidth $ \codelX ->
      colorToCodel additionalColor' $ getCodelColor multicoloredCodel' codelSizeInt image codelX codelY
  where
    pixelWidth = imageWidth image
    pixelHeight = imageHeight image
    codelSizeInt = getIntCodelSize (pixelWidth, pixelHeight) image (codelSize config)
    additionalColor' = additionalColor config
    multicoloredCodel' = multicoloredCodel config
    (codelWidth, modX) = divMod pixelWidth codelSizeInt
    (codelHeight, modY) = divMod pixelHeight codelSizeInt

getIntCodelSize ∷ Coordinates → Image PixelRGB8 → CodelSize → Int
getIntCodelSize _ _ (CodelSize n)         = n
getIntCodelSize size image GuessCodelSize = guessCodelSize size $ uncurry (pixelAt image)

getCodelColor ∷ MulticoloredCodelStrategy → Int → Image PixelRGB8 → Int → Int → PixelRGB8
getCodelColor MulticoloredCodelAsWhite codelSizeInt image codelX codelY
  | hasMultipleColors = PixelRGB8 0xFF 0xFF 0xFF
  | otherwise         = firstColor
  where
    hasMultipleColors = any (/= firstColor) colors
    firstColor = getFirstColor colors
    colors = getColors codelSizeInt image codelX codelY

getCodelColor MulticoloredCodelAsBlack codelSizeInt image codelX codelY
  | hasMultipleColors = PixelRGB8 0x00 0x00 0x00
  | otherwise         = firstColor
  where
    hasMultipleColors = any (/= firstColor) colors
    firstColor = getFirstColor colors
    colors = getColors codelSizeInt image codelX codelY

getCodelColor MulticoloredCodelCenter codelSizeInt image codelX codelY =
  pixelAt image (pixelOffsetX + codelSizeInt `div` 2) (pixelOffsetY + codelSizeInt `div` 2)
  where
    pixelOffsetX = codelX * codelSizeInt
    pixelOffsetY = codelY * codelSizeInt

getCodelColor MulticoloredCodelModal codelSizeInt image codelX codelY =
  selectModal (nonEmpty (NE.groupAllWith id colors))
  where
    colors = getColors codelSizeInt image codelX codelY
    selectModal (Just grouped) = head $ F1.maximumBy (comparing length) grouped
    selectModal Nothing        = getFirstColor colors

getCodelColor MulticoloredCodelAverage codelSizeInt image codelX codelY =
  PixelRGB8 (fromIntegral $ iR `div` codelsNum)
            (fromIntegral $ iG `div` codelsNum)
            (fromIntegral $ iB `div` codelsNum)
  where
    colors = getColors codelSizeInt image codelX codelY
    (iR, iG, iB) = foldl' (\(accR, accG, accB) (PixelRGB8 r g b) ->
                           (accR + fromIntegral r, accG + fromIntegral g, accB + fromIntegral b))
                         (0, 0, 0) colors
    codelsNum = toInteger $ codelSizeInt * codelSizeInt

-- Pomocnicze funkcje dla getCodelColor
getFirstColor ∷ [PixelRGB8] → PixelRGB8
getFirstColor (c : _) = c
getFirstColor []      = PixelRGB8 0 0 0

getColors ∷ Int → Image PixelRGB8 → Int → Int → [PixelRGB8]
getColors codelSizeInt image codelX codelY = do
  pixelY <- [pixelOffsetY .. pixelOffsetY + codelSizeInt - 1]
  pixelX <- [pixelOffsetX .. pixelOffsetX + codelSizeInt - 1]
  pure $ pixelAt image pixelX pixelY
  where
    pixelOffsetX = codelX * codelSizeInt
    pixelOffsetY = codelY * codelSizeInt

colorToCodel ∷ AdditionalColorStrategy → PixelRGB8 → Codel
colorToCodel AdditionalColorAsWhite color = M.findWithDefault White color colorCodelTable
colorToCodel AdditionalColorAsBlack color = M.findWithDefault Black color colorCodelTable
colorToCodel AdditionalColorNearest color = nearestCodel
  where
    squaredColorDistance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = square r1 r2 + square g1 g2 + square b1 b2
    square a b = (toInteger a - toInteger b) ^ (2 :: Int)
    nearestCodel = snd $ F1.minimum $ first (squaredColorDistance color) <$> colorCodelTableList

colorCodelTable ∷ M.Map PixelRGB8 Codel
colorCodelTable = M.fromList $ NE.toList colorCodelTableList

colorCodelTableList ∷ NonEmpty (PixelRGB8, Codel)
colorCodelTableList =
    (PixelRGB8 0xFF 0xFF 0xFF, White)
    :| [(PixelRGB8 0x00 0x00 0x00, Black)
      , (PixelRGB8 0xFF 0xC0 0xC0, Chromatic $ ChromaticColor Red Light)
      , (PixelRGB8 0xFF 0x00 0x00, Chromatic $ ChromaticColor Red Normal)
      , (PixelRGB8 0xC0 0x00 0x00, Chromatic $ ChromaticColor Red Dark)
      , (PixelRGB8 0xFF 0xFF 0xC0, Chromatic $ ChromaticColor Yellow Light)
      , (PixelRGB8 0xFF 0xFF 0x00, Chromatic $ ChromaticColor Yellow Normal)
      , (PixelRGB8 0xC0 0xC0 0x00, Chromatic $ ChromaticColor Yellow Dark)
      , (PixelRGB8 0xC0 0xFF 0xC0, Chromatic $ ChromaticColor Green Light)
      , (PixelRGB8 0x00 0xFF 0x00, Chromatic $ ChromaticColor Green Normal)
      , (PixelRGB8 0x00 0xC0 0x00, Chromatic $ ChromaticColor Green Dark)
      , (PixelRGB8 0xC0 0xFF 0xFF, Chromatic $ ChromaticColor Cyan Light)
      , (PixelRGB8 0x00 0xFF 0xFF, Chromatic $ ChromaticColor Cyan Normal)
      , (PixelRGB8 0x00 0xC0 0xC0, Chromatic $ ChromaticColor Cyan Dark)
      , (PixelRGB8 0xC0 0xC0 0xFF, Chromatic $ ChromaticColor Blue Light)
      , (PixelRGB8 0x00 0x00 0xFF, Chromatic $ ChromaticColor Blue Normal)
      , (PixelRGB8 0x00 0x00 0xC0, Chromatic $ ChromaticColor Blue Dark)
      , (PixelRGB8 0xFF 0xC0 0xFF, Chromatic $ ChromaticColor Magenta Light)
      , (PixelRGB8 0xFF 0x00 0xFF, Chromatic $ ChromaticColor Magenta Normal)
      , (PixelRGB8 0xC0 0x00 0xC0, Chromatic $ ChromaticColor Magenta Dark)
      ]
