module HelVM.HelMA.Automata.Piet.Parser (
  parseToRightTextIO,
  parseRightIO,
) where

import           HelVM.HelMA.Automata.Piet.Color
import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.Image

import           HelVM.HelMA.Automata.Piet.Common.MonadFailExtra

import           HelVM.HelIO.Extra

import           Control.Applicative.Tools
import           Control.Exception

import           Data.ListLike                                   (length)

import           Graphics.Imlib

import           Safe                                            (findJust)

import           Prelude                                         hiding (length)

parseToRightTextIO :: FilePath -> IO Text
parseToRightTextIO = showP <.> parseRightIO

parseRightIO :: FilePath -> IO ColorImage
parseRightIO = eitherToMonadFail <=< parseIO

parseIO :: FilePath -> ColorImageEitherIO
parseIO = imageFromFilePath Nothing

imageFromFilePath ::  Maybe Int -> FilePath -> ColorImageEitherIO
imageFromFilePath codelInfo = flip (uncurry buildColorImageEitherIO) codelInfo . swap <=< loadImageWithErrorReturn

buildColorImageEitherIO :: ImlibLoadError -> ImlibImage -> Maybe Int -> ColorImageEitherIO
buildColorImageEitherIO ImlibLoadErrorNone img codelInfo = Right <$> imageFromImlib img codelInfo
buildColorImageEitherIO err                _   _         = pure $ Left err

imageFromImlib :: ImlibImage -> Maybe Int -> IO ColorImage
imageFromImlib img = setImlibImage img . buildColorImageIO

setImlibImage :: ImlibImage -> IO a -> IO a
setImlibImage img = bracket_ (contextSetImage img) freeImageAndDecache

buildColorImageIO :: Maybe Int -> IO ColorImage
buildColorImageIO = imageFromContext <=< imageGuessCodelLength

imageGuessCodelLength :: Maybe Int -> IO Int
imageGuessCodelLength = maybe imageGuessCodelLengthDefault pure

imageGuessCodelLengthDefault :: IO Int
imageGuessCodelLengthDefault = do
  width  <- imageGetWidth
  height <- imageGetHeight
  rows   <- mapM (\y -> mapM (`imageQueryPixel` y) [0 .. width -1]) [0 .. height-1]
  cols   <- mapM (\x -> mapM ( imageQueryPixel  x) [0 .. height-1]) [0 .. width -1]
  pure $ imageGuessCodelLengthDefault' rows cols width height

imageGuessCodelLengthDefault' :: (Eq a1, Eq a2) => [a1] -> [a2] -> Int -> Int -> Int
imageGuessCodelLengthDefault' rows cols width height = findJust (== 1) $ scanl gcd (gcd width height) list
  where list = groupAndLength rows <> groupAndLength cols

groupAndLength :: Eq a => [a] -> [Int]
groupAndLength = length <.> group

imageFromContext :: Int -> IO ColorImage
imageFromContext = imageFromContextWithMax . max 1

imageFromContextWithMax :: Int -> IO ColorImage
imageFromContextWithMax codelLength = do
  width  <- (`div` codelLength) <$> imageGetWidth
  height <- (`div` codelLength) <$> imageGetHeight
  pixels <- mapM (toColors codelLength) [ (x, y) | x <- [ 0 .. width-1 ], y <- [ 0 .. height-1 ] ]
  pure $ newImage pixels (width-1 , height-1)

toColors :: Int -> Coordinates -> IO (Coordinates , Color)
toColors codelLength xy = (xy , ) . toColor <$> toImlibColorIO codelLength xy

toImlibColorIO :: Int -> (Int, Int) -> IO ImlibColor
toImlibColorIO codelLength (x , y) = imageQueryPixel (x * codelLength) (y * codelLength)

toColor :: ImlibColor -> Color
toColor (ImlibColor _ r g b) = rgb2Color $ RGBColor r g b

-- | Types

type ColorImageEitherIO = IO (Either ImlibLoadError ColorImage)
type ColorImage = Image Color
