module HelVM.HelMA.Automata.Piet.ImageLoader (
  loadPietImage,
  imageToPietColors,
  imageToPPMImage,
) where

import           Codec.Picture
import qualified Data.Vector.Storable             as VS
import           Data.Word                        (Word8)

import           HelVM.HelMA.Automata.Piet.Color  (Color, RGBColor (..), rgb2Color)
import           HelVM.HelMA.Automata.Piet.Parser (PPMImage (..))

-- | Load image from file (any format supported by JuicyPixels)
loadPietImage :: FilePath -> IO (Either String DynamicImage)
loadPietImage = readImage

-- | Convert DynamicImage to PPMImage
imageToPPMImage :: DynamicImage -> Either String PPMImage
imageToPPMImage (ImageRGB8 img)  = convertRGB8 img
imageToPPMImage (ImageRGBA8 img) = convertRGBA8 img
imageToPPMImage (ImageRGB16 img) = convertRGB16 img
imageToPPMImage (ImageGray8 img) = convertGray8 img
imageToPPMImage other            = Left $ "Unsupported image format: " ++ show other

-- | Convert 8-bit RGB image to PPMImage
convertRGB8 :: Image PixelRGB8 -> Either String PPMImage
convertRGB8 img = Right $ PPMImage
  { width = imageWidth img
  , height = imageHeight img
  , maxValue = 255
  , pixels = extractPixels img
  }
  where
    extractPixels :: Image PixelRGB8 -> [[Pixel]]
    extractPixels i =
      let w = imageWidth i
          h = imageHeight i
      in [[pixelAt i x y | x <- [0..w-1]] | y <- [0..h-1]]

    pixelAt :: Image PixelRGB8 -> Int -> Int -> Pixel
    pixelAt i x y =
      let PixelRGB8 r g b = pixelAtPoint i (fromIntegral x) (fromIntegral y)
      in Pixel r g b

-- | Convert RGBA image (drop alpha channel)
convertRGBA8 :: Image PixelRGBA8 -> Either String PPMImage
convertRGBA8 img = Right $ PPMImage
  { width = imageWidth img
  , height = imageHeight img
  , maxValue = 255
  , pixels = extractPixels img
  }
  where
    extractPixels :: Image PixelRGBA8 -> [[Pixel]]
    extractPixels i =
      let w = imageWidth i
          h = imageHeight i
      in [[pixelAt i x y | x <- [0..w-1]] | y <- [0..h-1]]

    pixelAt :: Image PixelRGBA8 -> Int -> Int -> Pixel
    pixelAt i x y =
      let PixelRGBA8 r g b _ = pixelAtPoint i (fromIntegral x) (fromIntegral y)
      in Pixel r g b

-- | Convert 16-bit RGB image
convertRGB16 :: Image PixelRGB16 -> Either String PPMImage
convertRGB16 img = Right $ PPMImage
  { width = imageWidth img
  , height = imageHeight img
  , maxValue = 65535
  , pixels = extractPixels img
  }
  where
    extractPixels :: Image PixelRGB16 -> [[Pixel]]
    extractPixels i =
      let w = imageWidth i
          h = imageHeight i
      in [[pixelAt i x y | x <- [0..w-1]] | y <- [0..h-1]]

    pixelAt :: Image PixelRGB16 -> Int -> Int -> Pixel
    pixelAt i x y =
      let PixelRGB16 r g b = pixelAtPoint i (fromIntegral x) (fromIntegral y)
      in Pixel (fromIntegral (r `div` 256))
               (fromIntegral (g `div` 256))
               (fromIntegral (b `div` 256))

-- | Convert grayscale image to RGB (replicate value)
convertGray8 :: Image Pixel8 -> Either String PPMImage
convertGray8 img = Right $ PPMImage
  { width = imageWidth img
  , height = imageHeight img
  , maxValue = 255
  , pixels = extractPixels img
  }
  where
    extractPixels :: Image Pixel8 -> [[Pixel]]
    extractPixels i =
      let w = imageWidth i
          h = imageHeight i
      in [[pixelAt i x y | x <- [0..w-1]] | y <- [0..h-1]]

    pixelAt :: Image Pixel8 -> Int -> Int -> Pixel
    pixelAt i x y =
      let v = pixelAtPoint i (fromIntegral x) (fromIntegral y)
      in Pixel v v v

-- | Convert image to Piet colors
imageToPietColors :: DynamicImage -> Either String [[Color]]
imageToPietColors img = do
  ppmImg <- imageToPPMImage img
  Right $ convertPixelsToColors (pixels ppmImg)
  where
    convertPixelsToColors :: [[Pixel]] -> [[Color]]
    convertPixelsToColors = map (map pixelToColor)

    pixelToColor :: Pixel -> Color
    pixelToColor (Pixel r g b) = rgb2Color (RGBColor r g b)

-- | Main workflow: Load image -> Convert to PPM -> Get colors
loadAndConvert :: FilePath -> IO (Either String PPMImage)
loadAndConvert filepath = do
  imgResult <- loadPietImage filepath
  pure $ case imgResult of
    Left err         -> Left err
    Right dynamicImg -> imageToPPMImage dynamicImg

-- | Load image and get Piet colors directly
loadPietColors :: FilePath -> IO (Either String [[Color]])
loadPietColors filepath = do
  imgResult <- loadPietImage filepath
  pure $ case imgResult of
    Left err         -> Left err
    Right dynamicImg -> imageToPietColors dynamicImg
