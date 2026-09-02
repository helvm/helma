{-# LANGUAGE ConstraintKinds  #-}
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

import           HelVM.HelMA.Automata.Piet.CodelSize
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.ToRGB8
import           HelVM.HelMA.Automata.Piet.Types.Color

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Codec.Picture

import           Control.Monad.Except                           ( MonadError (throwError), liftEither )

import qualified Data.Foldable1                                 as F1
import qualified Data.List.NonEmpty                             as NE
import qualified Data.Map                                       as M
import           Data.Vector                                    ( Vector )
import qualified Data.Vector                                    as V

data ImageReaderError
  = ReadImageFileError String
  | UnsupportedImageError String
  | CodelSizeError
  deriving stock (Eq, Show)

data AdditionalColorStrategy
  = AdditionalColorAsWhite
  | AdditionalColorAsBlack
  | AdditionalColorNearest
  deriving stock (Eq, Ord, Show)

data MulticoloredCodelStrategy
  = MulticoloredCodelAsWhite
  | MulticoloredCodelAsBlack
  | MulticoloredCodelCenter
  | MulticoloredCodelModal
  | MulticoloredCodelAverage
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

type MonadImageError m = MonadError ImageReaderError m

readCodels ∷ (MonadIO m, MonadImageError m) ⇒ ImageConfig → FilePath → m (Vector (Vector Color))
readCodels config = (=<<) (imageToCodels config) . (=<<) (liftEither . first ReadImageFileError) . liftIO . readImage

imageToCodels ∷ MonadImageError m ⇒ ImageConfig → DynamicImage → m (Vector (Vector Color))
imageToCodels config = (=<<) (rgbImageToCodels config) . liftEither . first UnsupportedImageError . toRGB8ImageM

rgbImageToCodels ∷ MonadImageError m ⇒ ImageConfig → Image PixelRGB8 → m (Vector (Vector Color))
rgbImageToCodels config image = checkDimensions modX modY *> pure (buildMatrix codelHeight codelWidth additionalColor' multicoloredCodel' codelSizeInt image) where
  pixelWidth = imageWidth image
  pixelHeight = imageHeight image
  codelSizeInt = getIntCodelSize (pixelWidth, pixelHeight) image (codelSize config)
  additionalColor' = additionalColor config
  multicoloredCodel' = multicoloredCodel config
  (codelWidth, modX) = divMod pixelWidth codelSizeInt
  (codelHeight, modY) = divMod pixelHeight codelSizeInt

checkDimensions ∷ MonadImageError m ⇒ Int → Int → m ()
checkDimensions 0 0 = pass
checkDimensions _ _ = throwError CodelSizeError

buildMatrix ∷ Int → Int → AdditionalColorStrategy → MulticoloredCodelStrategy → Int → Image PixelRGB8 → Vector (Vector Color)
buildMatrix codelHeight codelWidth stratMulti stratAdd sizeInt image = V.generate codelHeight (buildRow codelWidth stratMulti stratAdd sizeInt image)

buildRow ∷ Int → AdditionalColorStrategy → MulticoloredCodelStrategy → Int → Image PixelRGB8 → Int → Vector Color
buildRow codelWidth stratMulti stratAdd sizeInt image codelY = V.generate codelWidth (buildCodel stratMulti stratAdd sizeInt image codelY)

buildCodel ∷ AdditionalColorStrategy → MulticoloredCodelStrategy → Int → Image PixelRGB8 → Int → Int → Color
buildCodel stratAdd stratMulti sizeInt image codelY codelX = colorToCodel stratAdd (getCodelColor stratMulti sizeInt image codelX codelY)

getIntCodelSize ∷ Coordinates → Image PixelRGB8 → CodelSize → Int
getIntCodelSize _ _ (CodelSize n)         = n
getIntCodelSize size image GuessCodelSize = guessCodelSize size (uncurry (pixelAt image))

getCodelColor ∷ MulticoloredCodelStrategy → Int → Image PixelRGB8 → Int → Int → PixelRGB8
getCodelColor MulticoloredCodelAsWhite codelSizeInt image codelX codelY = handleWhiteStrategy (getColors codelSizeInt image codelX codelY)
getCodelColor MulticoloredCodelAsBlack codelSizeInt image codelX codelY = handleBlackStrategy (getColors codelSizeInt image codelX codelY)
getCodelColor MulticoloredCodelCenter codelSizeInt image codelX codelY = pixelAt image (pixelOffsetX + codelSizeInt `div` 2) (pixelOffsetY + codelSizeInt `div` 2) where
  pixelOffsetX = codelX * codelSizeInt
  pixelOffsetY = codelY * codelSizeInt
getCodelColor MulticoloredCodelModal codelSizeInt image codelX codelY = selectModal (nonEmpty (NE.groupAllWith id colors)) colors where
  colors = getColors codelSizeInt image codelX codelY
getCodelColor MulticoloredCodelAverage codelSizeInt image codelX codelY = makeAveragePixel iR iG iB codelsNum where
  colors = getColors codelSizeInt image codelX codelY
  (iR, iG, iB) = foldl' accumulateRGB (0, 0, 0) colors
  codelsNum = toInteger (codelSizeInt * codelSizeInt)

handleWhiteStrategy ∷ [PixelRGB8] → PixelRGB8
handleWhiteStrategy colors = checkMultipleWhite (any (/= firstColor) colors) firstColor where
  firstColor = getFirstColor colors

checkMultipleWhite ∷ Bool → PixelRGB8 → PixelRGB8
checkMultipleWhite True _           = PixelRGB8 0xFF 0xFF 0xFF
checkMultipleWhite False firstColor = firstColor

handleBlackStrategy ∷ [PixelRGB8] → PixelRGB8
handleBlackStrategy colors = checkMultipleBlack (any (/= firstColor) colors) firstColor where
  firstColor = getFirstColor colors

checkMultipleBlack ∷ Bool → PixelRGB8 → PixelRGB8
checkMultipleBlack True _           = PixelRGB8 0x00 0x00 0x00
checkMultipleBlack False firstColor = firstColor

selectModal ⠷ Maybe (NonEmpty (NonEmpty PixelRGB8)) → [PixelRGB8] → PixelRGB8
selectModal (Just grouped) _      = head (F1.maximumBy (comparing length) grouped)
selectModal Nothing        colors = getFirstColor colors

makeAveragePixel ∷ Integer → Integer → Integer → Integer → PixelRGB8
makeAveragePixel iR iG iB codelsNum = PixelRGB8 (fromIntegral (iR `div` codelsNum)) (fromIntegral (iG `div` codelsNum)) (fromIntegral (iB `div` codelsNum))

accumulateRGB ∷ (Integer, Integer, Integer) → PixelRGB8 → (Integer, Integer, Integer)
accumulateRGB (accR, accG, accB) (PixelRGB8 r g b) = (accR + fromIntegral r, accG + fromIntegral g, accB + fromIntegral b)

getFirstColor ∷ [PixelRGB8] → PixelRGB8
getFirstColor (c : _) = c
getFirstColor []      = PixelRGB8 0 0 0

getColors ∷ Int → Image PixelRGB8 → Int → Int → [PixelRGB8]
getColors codelSizeInt image codelX codelY = pixelAt image <$> [pixelOffsetX .. pixelOffsetX + codelSizeInt - 1] <*> [pixelOffsetY .. pixelOffsetY + codelSizeInt - 1] where
  pixelOffsetX = codelX * codelSizeInt
  pixelOffsetY = codelY * codelSizeInt

colorToCodel ∷ AdditionalColorStrategy → PixelRGB8 → Color
colorToCodel AdditionalColorAsWhite color = M.findWithDefault White color colorCodelTable
colorToCodel AdditionalColorAsBlack color = M.findWithDefault Black color colorCodelTable
colorToCodel AdditionalColorNearest color = nearestCodel color

nearestCodel ∷ PixelRGB8 → Color
nearestCodel color = snd (F1.minimum (fmap (first (squaredColorDistance color)) colorCodelTableList))

squaredColorDistance ∷ PixelRGB8 → PixelRGB8 → Integer
squaredColorDistance (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) = square r1 r2 + square g1 g2 + square b1 b2

square ∷ Word8 → Word8 → Integer
square a b = (toInteger a - toInteger b) ^ (2 ∷ Int)

colorCodelTable ∷ M.Map PixelRGB8 Color
colorCodelTable = M.fromList (NE.toList colorCodelTableList)

colorCodelTableList ∷ NonEmpty (PixelRGB8, Color)
colorCodelTableList =
  PixelRGB8 0xFF 0xFF 0xFF, White
  :| [ PixelRGB8 0x00 0x00 0x00, Black
     , PixelRGB8 0xFF 0xC0 0xC0, Chromatic (ChromaticColor Red Light)
     , PixelRGB8 0xFF 0x00 0x00, Chromatic (ChromaticColor Red Normal)
     , PixelRGB8 0xC0 0x00 0x00, Chromatic (ChromaticColor Red Dark)
     , PixelRGB8 0xFF 0xFF 0xC0, Chromatic (ChromaticColor Yellow Light)
     , PixelRGB8 0xFF 0xFF 0x00, Chromatic (ChromaticColor Yellow Normal)
     , PixelRGB8 0xC0 0xC0 0x00, Chromatic (ChromaticColor Yellow Dark)
     , PixelRGB8 0xC0 0xFF 0xC0, Chromatic (ChromaticColor Green Light)
     , PixelRGB8 0x00 0xFF 0x00, Chromatic (ChromaticColor Green Normal)
     , PixelRGB8 0x00 0xC0 0x00, Chromatic (ChromaticColor Green Dark)
     , PixelRGB8 0xC0 0xFF 0xFF, Chromatic (ChromaticColor Cyan Light)
     , PixelRGB8 0x00 0xFF 0xFF, Chromatic (ChromaticColor Cyan Normal)
     , PixelRGB8 0x00 0xC0 0xC0, Chromatic (ChromaticColor Cyan Dark)
     , PixelRGB8 0xC0 0xC0 0xFF, Chromatic (ChromaticColor Blue Light)
     , PixelRGB8 0x00 0x00 0xFF, Chromatic (ChromaticColor Blue Normal)
     , PixelRGB8 0x00 0x00 0xC0, Chromatic (ChromaticColor Blue Dark)
     , PixelRGB8 0xFF 0xC0 0xFF, Chromatic (ChromaticColor Magenta Light)
     , PixelRGB8 0xFF 0x00 0xFF, Chromatic (ChromaticColor Magenta Normal)
     , PixelRGB8 0xC0 0x00 0xC0, Chromatic (ChromaticColor Magenta Dark)
     ]
