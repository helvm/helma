{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelMA.Automata.Piet.Parser (
  parsePPM,
  PPMImage(..),
  Pixel(..),
) where

import qualified Data.Text.Lazy as LT
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace, isDigit)
import Control.Monad (unless)

-- | Piet pixel representation (RGB)
data Pixel = Pixel
  { pixelRed   :: Int
  , pixelGreen :: Int
  , pixelBlue  :: Int
  } deriving (Show, Eq)

-- | PPM image structure
data PPMImage = PPMImage
  { width    :: Int
  , height   :: Int
  , maxValue :: Int
  , pixels   :: [[Pixel]]  -- matrix of pixels
  } deriving (Show, Eq)

-- | Main parser entry point
parsePPM :: String -> Either String PPMImage
parsePPM content = do
  lines' <- pure $ lines content
  (magic, rest1) <- parseMagic lines'
  unless (magic == "P3") $ Left "Only P3 (ASCII) PPM format is supported"
  (w, h, maxVal, pixelLines) <- parseHeader rest1
  parsePixels w h maxVal pixelLines

-- | Parse magic number (P3 for ASCII PPM)
parseMagic :: [String] -> Either String (String, [String])
parseMagic [] = Left "Empty file"
parseMagic (l:ls) =
  let trimmed = dropWhile isSpace l
  in if take 2 trimmed == "P3"
     then Right ("P3", ls)
     else Left $ "Invalid magic number: " ++ take 2 trimmed

-- | Parse header (width, height, max value) and skip comments
parseHeader :: [String] -> Either String (Int, Int, Int, [String])
parseHeader ls = do
  let cleanLines = skipComments ls
  case cleanLines of
    [] -> Left "Incomplete header"
    (wLine:rest1) -> do
      w <- parseInteger (words wLine)
      case skipComments rest1 of
        [] -> Left "Incomplete header"
        (hLine:rest2) -> do
          h <- parseInteger (words hLine)
          case skipComments rest2 of
            [] -> Left "Incomplete header"
            (mLine:rest3) -> do
              maxVal <- parseInteger (words mLine)
              unless (maxVal > 0) $ Left "Invalid max value"
              Right (w, h, maxVal, rest3)

-- | Skip comment lines (starting with #)
skipComments :: [String] -> [String]
skipComments = filter (not . isComment)
  where
    isComment "" = False
    isComment s = head (dropWhile isSpace s) == '#'

-- | Parse single integer from word list
parseInteger :: [String] -> Either String Int
parseInteger [] = Left "Missing integer value"
parseInteger (w:_) =
  if all isDigit w
  then Right (read w)
  else Left $ "Not an integer: " ++ w

-- | Parse all pixels from remaining lines
parsePixels :: Int -> Int -> Int -> [String] -> Either String PPMImage
parsePixels w h maxVal pixelLines = do
  let pixelStr = unwords pixelLines
  let pixelWords = words pixelStr
  let pixelCount = w * h * 3  -- each pixel has 3 components (RGB)
  unless (length pixelWords >= pixelCount) $
    Left $ "Not enough pixel data: expected " ++ show pixelCount ++
           " values, got " ++ show (length pixelWords)

  let (pixelValues, _) = splitAt pixelCount pixelWords
  intValues <- mapM (parsePixelValue maxVal) pixelValues
  let pixelList = chunksOf 3 intValues
  let pixelMatrix = chunksOf w (map toPixel pixelList)

  unless (length pixelMatrix == h) $
    Left $ "Wrong number of rows: expected " ++ show h ++
           ", got " ++ show (length pixelMatrix)

  Right $ PPMImage w h maxVal pixelMatrix

-- | Parse individual pixel value (0-maxValue)
parsePixelValue :: Int -> String -> Either String Int
parsePixelValue maxVal str
  | all isDigit str =
      let val = read str
      in if val >= 0 && val <= maxVal
         then Right val
         else Left $ "Pixel value out of range: " ++ str ++
                     " (max: " ++ show maxVal ++ ")"
  | otherwise = Left $ "Invalid pixel value: " ++ str

-- | Convert three integers to Pixel
toPixel :: [Int] -> Pixel
toPixel [r, g, b] = Pixel r g b
toPixel _ = error "Invalid pixel data"

-- | Split list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Utility: get pixel at coordinates
getPixel :: PPMImage -> Int -> Int -> Maybe Pixel
getPixel img x y
  | x >= 0 && x < width img && y >= 0 && y < height img =
      Just $ pixels img !! y !! x
  | otherwise = Nothing

-- | Print PPM image info
pprintPPM :: PPMImage -> String
pprintPPM img = unlines
  [ "PPM Image:"
  , "  Width:    " ++ show (width img)
  , "  Height:   " ++ show (height img)
  , "  Max Value: " ++ show (maxValue img)
  , "  Total pixels: " ++ show (width img * height img)
  ]
