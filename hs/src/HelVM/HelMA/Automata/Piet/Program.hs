module HelVM.HelMA.Automata.Piet.Program where

import           HelVM.HelMA.Automata.Piet.Color
import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.EquivalenceMap
import           HelVM.HelMA.Automata.Piet.FullInfo
import           HelVM.HelMA.Automata.Piet.Image
import qualified HelVM.HelMA.Automata.Piet.LabellingStatus as LS

import           Data.IntMap                               (alter, foldrWithKey)

compile :: Image Color -> Program
compile image_ = Program
  { image = image_
  , mask  = mask
  , info  = info_
  }
  where (mask, info_)  = label4 image_

label4 :: Eq a => Image a -> (Image LabelKey, IntMap LabelInfo)
label4 = label4With (==)

label4With :: (a -> a -> Bool) -> Image a -> (Image LabelKey, IntMap LabelInfo)
label4With neighbours img = (img', inf) where
  img'  = fmap (`equivClass` (LS.equivalences status)) $ LS.mask status
  inf  = foldrWithKey (updateMask status) mempty $ LS.infoMap status
  status  = label4With' neighbours img $ LS.newLabellingStatus img

updateMask :: Monoid a => LS.LabellingStatus -> LabelKey -> a -> IntMap a -> IntMap a
updateMask status label labelInfo = alter (maybe (Just labelInfo) (Just . mappend labelInfo)) label' where
  label' = equivClass label $ LS.equivalences status

label4With' :: (a -> a -> Bool) -> Image a -> LS.LabellingStatus -> LS.LabellingStatus
label4With' neighbours img status = let
  xy          = LS.currentCoordinates status
  pixel       = unsafeLoopUp xy img
  mergeLabels = LS.buildLabelKeys neighbours img status pixel
  status'     = LS.updateStatus mergeLabels xy status
  in case nextCoords xy of
    Just xy' -> label4With' neighbours img $ status' { LS.currentCoordinates = xy' }
    Nothing  -> status'

  where
  nextCoords :: Coordinates -> Maybe Coordinates
  nextCoords = nextCoordinates $ border img

data Program = Program
  { image :: Image Color
  , mask  :: Image LabelKey
  , info  :: IntMap LabelInfo
  }

type Update a = a -> a
