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

import           Codec.Picture
import           Control.Monad.Except
import           Data.List                                         ( maximumBy, minimum )
import qualified Data.Map                                          as M
import           Data.Vector                                       ( Vector )
import qualified Data.Vector                                       as V
import           GHC.Exts
import           HelVM.HelMA.Automata.Piet.LLVM.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.CodelSize
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.ToRGB8

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
  let
    pixelWidth = imageWidth image
    pixelHeight = imageHeight image
    codelSizeInt = getIntCodelSize (pixelWidth, pixelHeight) image (codelSize config)
    additionalColor' = additionalColor config
    multicoloredCodel' = multicoloredCodel config
    (codelWidth, modX) = divMod pixelWidth codelSizeInt
    (codelHeight, modY) = divMod pixelHeight codelSizeInt
  when (modX /= 0 || modY /= 0) $ throwError CodelSizeError
  return $ V.generate codelHeight $ \codelY ->
    V.generate codelWidth $ \codelX ->
      colorToCodel additionalColor' $ getCodelColor multicoloredCodel' codelSizeInt image codelX codelY

getIntCodelSize ∷ (Int, Int) → Image PixelRGB8 → CodelSize → Int
getIntCodelSize _ _ (CodelSize n)         = n
getIntCodelSize size image GuessCodelSize = guessCodelSize size $ uncurry (pixelAt image)

getCodelColor ∷ MulticoloredCodelStrategy → Int → Image PixelRGB8 → Int → Int → PixelRGB8
getCodelColor strategy codelSizeInt image codelX codelY = getCodelColor' strategy where
  getCodelColor' MulticoloredCodelAsWhite = if hasMultipleColors then PixelRGB8 0xFF 0xFF 0xFF else firstColor
  getCodelColor' MulticoloredCodelAsBlack = if hasMultipleColors then PixelRGB8 0x00 0x00 0x00 else firstColor
  getCodelColor' MulticoloredCodelCenter = pixelAt image (pixelOffsetX + codelSizeInt `div` 2) (pixelOffsetY + codelSizeInt `div` 2)
  getCodelColor' MulticoloredCodelModal = getHead $ maximumBy (comparing length) $ groupWith id colors
  getCodelColor' MulticoloredCodelAverage = average where
    average = PixelRGB8 (toP $ iR `div` codelsNum) (toP $ iG `div` codelsNum) (toP $ iB `div` codelsNum)
    (iR, iG, iB) = foldl' (\(accR, accG, accB) (PixelRGB8 r g b) -> (accR + toI r, accG + toI g, accB + toI b)) (0, 0, 0) colors
    toI = toInteger . fromEnum
    toP = toEnum . fromInteger
    codelsNum = toInteger $ codelSizeInt * codelSizeInt
  hasMultipleColors = any (/= firstColor) colors
  firstColor = getHead colors
  getHead (c : _) = c
  getHead []      = error "unreachable"
  colors = do
    pixelY <- (+ pixelOffsetY) <$> [0 .. codelSizeInt - 1]
    pixelX <- (+ pixelOffsetX) <$> [0 .. codelSizeInt - 1]
    return $ pixelAt image pixelX pixelY
  pixelOffsetX = codelX * codelSizeInt
  pixelOffsetY = codelY * codelSizeInt

colorToCodel ∷ AdditionalColorStrategy → PixelRGB8 → Codel
colorToCodel AdditionalColorAsWhite color = M.findWithDefault WhiteCodel color colorCodelTable
colorToCodel AdditionalColorAsBlack color = M.findWithDefault BlackCodel color colorCodelTable
colorToCodel AdditionalColorNearest color = nearestCodel where
  squaredColorDistance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = square r1 r2 + square g1 g2 + square b1 b2
  square a b = (toInteger a - toInteger b) ^ (2 :: Int)
  nearestCodel = snd $ minimum $ first (squaredColorDistance color) <$> colorCodelTableList

colorCodelTable ∷ Map PixelRGB8 Codel
colorCodelTable = M.fromList colorCodelTableList

colorCodelTableList ∷ [(PixelRGB8, Codel)]
colorCodelTableList = [ (PixelRGB8 0xFF 0xC0 0xC0, AchromaticCodel Red Light)
                      , (PixelRGB8 0xFF 0x00 0x00, AchromaticCodel Red Normal)
                      , (PixelRGB8 0xC0 0x00 0x00, AchromaticCodel Red Dark)
                      , (PixelRGB8 0xFF 0xFF 0xC0, AchromaticCodel Yellow Light)
                      , (PixelRGB8 0xFF 0xFF 0x00, AchromaticCodel Yellow Normal)
                      , (PixelRGB8 0xC0 0xC0 0x00, AchromaticCodel Yellow Dark)
                      , (PixelRGB8 0xC0 0xFF 0xC0, AchromaticCodel Green Light)
                      , (PixelRGB8 0x00 0xFF 0x00, AchromaticCodel Green Normal)
                      , (PixelRGB8 0x00 0xC0 0x00, AchromaticCodel Green Dark)
                      , (PixelRGB8 0xC0 0xFF 0xFF, AchromaticCodel Cyan Light)
                      , (PixelRGB8 0x00 0xFF 0xFF, AchromaticCodel Cyan Normal)
                      , (PixelRGB8 0x00 0xC0 0xC0, AchromaticCodel Cyan Dark)
                      , (PixelRGB8 0xC0 0xC0 0xFF, AchromaticCodel Blue Light)
                      , (PixelRGB8 0x00 0x00 0xFF, AchromaticCodel Blue Normal)
                      , (PixelRGB8 0x00 0x00 0xC0, AchromaticCodel Blue Dark)
                      , (PixelRGB8 0xFF 0xC0 0xFF, AchromaticCodel Magenta Light)
                      , (PixelRGB8 0xFF 0x00 0xFF, AchromaticCodel Magenta Normal)
                      , (PixelRGB8 0xC0 0x00 0xFF, AchromaticCodel Magenta Dark)
                      , (PixelRGB8 0xFF 0xFF 0xFF, WhiteCodel)
                      , (PixelRGB8 0x00 0x00 0x00, BlackCodel)
                      ]
