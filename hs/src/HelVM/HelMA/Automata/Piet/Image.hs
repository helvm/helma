module HelVM.HelMA.Automata.Piet.Image where

import           HelVM.HelMA.Automata.Piet.Coordinates

import           Data.Array.IArray                     hiding (index)

import qualified Relude.Extra                          as Extra

-- | Constructors

copyEmptyImage :: Image a1 -> Image a2
copyEmptyImage = newEmptyImage . border

newEmptyImage :: Coordinates -> Image a
newEmptyImage = newImage []

newImage :: [(Coordinates, a)] -> Coordinates -> Image a
newImage entries c = array ((0 , 0) , c) entries

imgSetPixel :: Coordinates -> a -> Image a -> Image a
imgSetPixel c pixel img = img // [(c, pixel)]

-- | Getters
indexWithCoordinates :: Image a -> Coordinates -> (Coordinates , a)
indexWithCoordinates img = Extra.toSnd (index img)

index :: Image a -> Coordinates -> a
index = (!)

unsafeLoopUp :: Coordinates -> Image a -> a
unsafeLoopUp = flip (!)

border :: Image a -> Coordinates
border = snd . bounds

-- | Type
type Image a = Array Coordinates a
