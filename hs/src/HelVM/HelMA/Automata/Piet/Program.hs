module HelVM.HelMA.Automata.Piet.Program where

import           HelVM.HelMA.Automata.Piet.Color
import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.EquivalenceMap
import           HelVM.HelMA.Automata.Piet.FullInfo
import           HelVM.HelMA.Automata.Piet.Image
import           HelVM.HelMA.Automata.Piet.LabellingStatus

import           Data.IntMap                               (alter, foldrWithKey)

compile :: Image Color -> Program
compile image_ = Program
  { image = image_
  , mask  = mask_
  , info  = info_
  }
  where (mask_, info_)  = label4 image_

label4 :: Eq a => Image a -> (Image LabelKey, IntMap LabelInfo)
label4 = label4With (==)

label4With :: (a -> a -> Bool) -> Image a -> (Image LabelKey, IntMap LabelInfo)
label4With neighbours img = (img', inf) where
  img'  = fmap (`equivClass` (_equivalences status)) $ _mask status
  inf  = foldrWithKey (updateMask status) mempty $ _infoMap status
  status  = label4With' neighbours img $ newLabellingStatus img

updateMask :: Monoid a => LabellingStatus -> LabelKey -> a -> IntMap a -> IntMap a
updateMask status label labelInfo = alter (maybe (Just labelInfo) (Just . mappend labelInfo)) label' where
  label' = equivClass label $ _equivalences status

label4With' :: (a -> a -> Bool) -> Image a -> LabellingStatus -> LabellingStatus
label4With' neighbours img status = let
  xy          = _currentCoords status
  pixel       = unsafeLoopUp xy img
  mergeLabels = buildLabelKeys neighbours img status pixel
  status'     = updateStatus mergeLabels xy status
  in case nextCoords xy of
    Just xy' -> label4With' neighbours img $ status' { _currentCoords = xy' }
    Nothing  -> status'

  where
  nextCoords :: Coordinates -> Maybe Coordinates
  nextCoords = nextCoordinates $ border img

buildLabelKeys :: (t -> b -> Bool) -> Image b -> LabellingStatus -> t -> [LabelKey]
buildLabelKeys neighbours img status pixel
  = fmap (\(xy' , _) -> unsafeLoopUp xy' (_mask status))
  $ filter (\(_, e) -> neighbours pixel e)
  $ fmap (\xy' -> (xy' , unsafeLoopUp xy' img))
  $ previousNeighbours (_currentCoords status)

--aaa (Just xy') = label4With' neighbours img $ status' { _currentCoords = xy' }
--aaa (Nothing)  = status'

data Program = Program
  { image :: Image Color
  , mask  :: Image LabelKey
  , info  :: IntMap LabelInfo
  }

type Update a = a -> a
