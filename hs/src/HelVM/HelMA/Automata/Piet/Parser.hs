{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelMA.Automata.Piet.Parser (
  parsePPM,
  PPMImage(..),
  Pixel(..),
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import Data.Char (isSpace, isDigit)
import Control.Monad (unless)
import Data.List (stripPrefix)

-- | Piet pixel representation (RGB)
data Pixel = Pixel
  { pixelRed   :: Word8
  , pixelGreen :: Word8
  , pixelBlue  :: Word8
  } deriving (Show, Eq)

-- | PPM image structure
data PPMImage = PPMImage
  { width    :: Int
  , height   :: Int
  , maxValue :: Int
  , pixels   :: [[Pixel]]  -- matrix of pixels
  } deriving (Show, Eq)

-- | Main parser entry point - handles both P3 (ASCII) and P6 (binary)
parsePPM :: String -> Either String PPMImage
parsePPM = parsePPMBinary . BSC.pack

-- | Parse PPM from ByteString (handles P6 binary format)
parsePPMBinary :: BS.ByteString -> Either String PPMImage
parsePPMBinary bs = do
  (magic, rest1) <- parseMagicBinary bs
  case magic of
    "P6" -> parseHeaderAndPixelsBinary rest1
    "P3" -> parseHeaderAndPixelsASCII (BSC.unpack rest1)
    fmt  -> Left $ "Unsupported PPM format: " ++ fmt

-- | Parse magic number from ByteString
parseMagicBinary :: BS.ByteString -> Either String (String, BS.ByteString)
parseMagicBinary BS.Empty = Left "Empty file"
parseMagicBinary bs =
  case (BS.take 1 bs == BSC.pack "P", BS.length bs >= 2) of
    (True, True) -> Right (BSC.unpack (BS.take 2 bs), BS.drop 1 bs)
    (True, _)    -> Left "Incomplete magic number"
    _            -> Left $ "Invalid magic number: " ++ BSC.unpack (BS.take 2 bs)

-- | Parse P6 (binary) format header and pixels
parseHeaderAndPixelsBinary :: BS.ByteString -> Either String PPMImage
parseHeaderAndPixelsBinary bs = do
  (w, h, maxVal, pixelData) <- parseHeaderBinary bs
  case maxVal > 0 of
    False -> Left "Invalid max value"
    True  -> do
      let pixelCount = w * h
      let bytesPerPixel = if maxVal <= 255 then 3 else 6
      let expectedBytes = pixelCount * bytesPerPixel

      case BS.length pixelData >= expectedBytes of
        False -> Left $ "Not enough pixel data: expected " ++ show expectedBytes ++
                        " bytes, got " ++ show (BS.length pixelData)
        True  -> do
          let pixelBytes = BS.take expectedBytes pixelData
          pixelList <- parsePixelDataBinary maxVal bytesPerPixel pixelBytes
          let pixelMatrix = chunksOf w pixelList

          case length pixelMatrix == h of
            False -> Left $ "Wrong number of rows: expected " ++ show h ++
                            ", got " ++ show (length pixelMatrix)
            True  -> Right $ PPMImage w h maxVal pixelMatrix

-- | Parse header for P6 format (binary)
parseHeaderBinary :: BS.ByteString -> Either String (Int, Int, Int, BS.ByteString)
parseHeaderBinary bs = do
  let (wStr, rest1) = getNextToken bs
  w <- parseIntFromBS wStr
  let (hStr, rest2) = getNextToken rest1
  h <- parseIntFromBS hStr
  let (mvStr, pixelData) = getNextToken rest2
  maxVal <- parseIntFromBS mvStr
  Right (w, h, maxVal, pixelData)

-- | Get next whitespace-delimited token and skip comments
getNextToken :: BS.ByteString -> (BS.ByteString, BS.ByteString)
getNextToken = skipWhitespaceAndComments . skipWhitespaceAndComments
  where
    skipWhitespaceAndComments BS.Empty = ("", "")
    skipWhitespaceAndComments input
      | BSC.head input == '#' =
          let (_, rest) = BSC.breakSubstring "\n" input
          in skipWhitespaceAndComments (case rest of
               "" -> ""
               _  -> BS.tail rest)
      | isSpace (BSC.head input) =
          skipWhitespaceAndComments (BS.dropWhile isSpace input)
      | otherwise = BS.break (\c -> isSpace c || c == '\n') input

-- | Parse integer from ByteString
parseIntFromBS :: BS.ByteString -> Either String Int
parseIntFromBS BS.Empty = Left "Missing integer value"
parseIntFromBS bs =
  let str = BSC.unpack bs
  in case all isDigit str of
    True  -> Right (read str)
    False -> Left $ "Not an integer: " ++ str

-- | Parse pixel data (binary format, 3 or 6 bytes per pixel)
parsePixelDataBinary :: Int -> Int -> BS.ByteString -> Either String [Pixel]
parsePixelDataBinary _ _ BS.Empty = Right []
parsePixelDataBinary maxVal bytesPerPixel bs
  | BS.length bs < bytesPerPixel =
      Left $ "Incomplete pixel data: expected " ++ show bytesPerPixel ++
             " bytes, got " ++ show (BS.length bs)
parsePixelDataBinary maxVal bytesPerPixel bs =
  let (pixelBytes, rest) = BS.splitAt bytesPerPixel bs
      pixel = case bytesPerPixel of
        3 -> parsePixel3Bytes pixelBytes
        _ -> parsePixel6Bytes pixelBytes
  in (pixel :) <$> parsePixelDataBinary maxVal bytesPerPixel rest

-- | Parse 3-byte pixel (8-bit R, G, B)
parsePixel3Bytes :: BS.ByteString -> Pixel
parsePixel3Bytes bs
  | BS.length bs >= 3 = Pixel (BS.index bs 0) (BS.index bs 1) (BS.index bs 2)
  | otherwise = error "Invalid pixel data"

-- | Parse 6-byte pixel (16-bit R, G, B)
parsePixel6Bytes :: BS.ByteString -> Pixel
parsePixel6Bytes bs
  | BS.length bs >= 6 =
      Pixel (fromIntegral (BS.index bs 0) :: Word8)
            (fromIntegral (BS.index bs 2) :: Word8)
            (fromIntegral (BS.index bs 4) :: Word8)
  | otherwise = error "Invalid pixel data"

-- | Parse P3 (ASCII) format - fallback for compatibility
parseHeaderAndPixelsASCII :: String -> Either String PPMImage
parseHeaderAndPixelsASCII content = do
  let lines' = lines content
  (w, h, maxVal, pixelLines) <- parseHeaderASCII lines'
  parsePixelsASCII w h maxVal pixelLines

-- | Parse header for P3 format (ASCII)
parseHeaderASCII :: [String] -> Either String (Int, Int, Int, [String])
parseHeaderASCII ls =
  let cleanLines = skipCommentsASCII ls
  in case cleanLines of
    [] -> Left "Incomplete header"
    (wLine:rest1) -> do
      w <- parseIntegerASCII (words wLine)
      case skipCommentsASCII rest1 of
        [] -> Left "Incomplete header"
        (hLine:rest2) -> do
          h <- parseIntegerASCII (words hLine)
          case skipCommentsASCII rest2 of
            [] -> Left "Incomplete header"
            (mLine:rest3) -> do
              maxVal <- parseIntegerASCII (words mLine)
              case maxVal > 0 of
                False -> Left "Invalid max value"
                True  -> Right (w, h, maxVal, rest3)

-- | Skip comment lines for ASCII format
skipCommentsASCII :: [String] -> [String]
skipCommentsASCII = filter (not . isCommentASCII)
  where
    isCommentASCII "" = False
    isCommentASCII s = head (dropWhile isSpace s) == '#'

-- | Parse integer for ASCII format
parseIntegerASCII :: [String] -> Either String Int
parseIntegerASCII [] = Left "Missing integer value"
parseIntegerASCII (w:_) =
  case all isDigit w of
    True  -> Right (read w)
    False -> Left $ "Not an integer: " ++ w

-- | Parse all pixels from ASCII format
parsePixelsASCII :: Int -> Int -> Int -> [String] -> Either String PPMImage
parsePixelsASCII w h maxVal pixelLines = do
  let pixelStr = unwords pixelLines
  let pixelWords = words pixelStr
  let pixelCount = w * h * 3
  case length pixelWords >= pixelCount of
    False -> Left $ "Not enough pixel data: expected " ++ show pixelCount ++
                    " values, got " ++ show (length pixelWords)
    True  -> do
      let (pixelValues, _) = splitAt pixelCount pixelWords
      intValues <- mapM (parsePixelValueASCII maxVal) pixelValues
      let pixelList = map word8ify $ chunksOf 3 intValues
      let pixelMatrix = chunksOf w pixelList
      case length pixelMatrix == h of
        False -> Left $ "Wrong number of rows: expected " ++ show h ++
                        ", got " ++ show (length pixelMatrix)
        True  -> Right $ PPMImage w h maxVal pixelMatrix

-- | Parse individual pixel value for ASCII format
parsePixelValueASCII :: Int -> String -> Either String Int
parsePixelValueASCII maxVal str =
  case all isDigit str of
    False -> Left $ "Invalid pixel value: " ++ str
    True  ->
      let val = read str
      in case val >= 0 && val <= maxVal of
        False -> Left $ "Pixel value out of range: " ++ str ++
                        " (max: " ++ show maxVal ++ ")"
        True  -> Right val

-- | Convert Int to Word8 for pixel
word8ify :: [Int] -> Pixel
word8ify [r, g, b] = Pixel (fromIntegral r) (fromIntegral g) (fromIntegral b)
word8ify _ = error "Invalid pixel data"

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