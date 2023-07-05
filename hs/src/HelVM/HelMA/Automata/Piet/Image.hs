module HelVM.HelMA.Automata.Piet.Image where

import           Data.Array.IArray
import           HelVM.HelMA.Automata.Piet.Coordinates

-- | Constructors

newEmptyImage :: Coordinates -> Image a
newEmptyImage = newImage []

newImage :: [(Coordinates, a)] -> Coordinates -> Image a
newImage entries c = array ((0 , 0) , c) entries

imgSetPixel :: Coordinates -> a -> Image a -> Image a
imgSetPixel c pixel img = img // [(c, pixel)]

-- | Getters
unsafeLoopUp :: Coordinates -> Image a -> a
unsafeLoopUp = flip (!)

border :: Image a -> Coordinates
border = snd . bounds

-- | Type
type Image a = Array Coordinates a
