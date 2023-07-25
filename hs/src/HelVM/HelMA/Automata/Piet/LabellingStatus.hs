module HelVM.HelMA.Automata.Piet.LabellingStatus where

import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.EquivalenceMap
import           HelVM.HelMA.Automata.Piet.FullInfo
import           HelVM.HelMA.Automata.Piet.Image

import qualified Relude.Extra                             as Extra

import           Data.IntMap                              (adjust, insert)

updateStatus :: [LabelKey]  -> Coordinates -> LabellingStatus -> LabellingStatus
updateStatus [] (x , y) status = status
  { _nextKey  = Extra.next label
  , mask_    = imgSetPixel (x , y) label (mask_ status)
  , _infoMap  = insert label (addPixel (x , y) mempty) (_infoMap status)
  } where
    label = _nextKey status
updateStatus [label] (x , y) status = status
  { mask_    = imgSetPixel (x , y) label (mask_ status)
  , _infoMap  = adjust (addPixel (x , y)) label (_infoMap status)
  }
updateStatus [l1, l2] (x , y) status = status
  { mask_    = imgSetPixel (x , y) label (mask_ status)
  , _infoMap  = adjust (addPixel (x, y)) label (_infoMap status)
  , _equivalences  = equivInsert l1 l2 (_equivalences status)
  } where
    label = max l1 l2
updateStatus _ _ _ = error "too many neighbours in updateStatus"

newLabellingStatus :: Image a -> LabellingStatus
newLabellingStatus img = LabellingStatus
  { _currentCoords = (0, 0)
  , _nextKey       = 0
  , mask_          = copyEmptyImage img
  , _infoMap       = mempty
  , _equivalences  = mempty
  }

data LabellingStatus = LabellingStatus
  { _currentCoords :: Coordinates
  , _nextKey       :: LabelKey
  , mask_          :: Image LabelKey
  , _infoMap       :: IntMap LabelInfo
  , _equivalences  :: EquivalenceMap
  } deriving stock (Show, Eq, Ord)
