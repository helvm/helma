module HelVM.HelMA.Automata.Piet.LabellingStatus where

import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.EquivalenceMap
import           HelVM.HelMA.Automata.Piet.FullInfo
import           HelVM.HelMA.Automata.Piet.Image

import qualified Relude.Extra                             as Extra

import           Data.IntMap                              (adjust, insert)

updateStatus :: [LabelKey]  -> Coordinates -> LabellingStatus -> LabellingStatus
updateStatus [] (x , y) status = status
  { nextKey  = Extra.next label
  , mask    = imgSetPixel (x , y) label (mask status)
  , infoMap  = insert label (addPixel (x , y) mempty) (infoMap status)
  } where
    label = nextKey status
updateStatus [label] (x , y) status = status
  { mask    = imgSetPixel (x , y) label (mask status)
  , infoMap  = adjust (addPixel (x , y)) label (infoMap status)
  }
updateStatus [l1, l2] (x , y) status = status
  { mask    = imgSetPixel (x , y) label (mask status)
  , infoMap  = adjust (addPixel (x, y)) label (infoMap status)
  , equivalences  = equivInsert l1 l2 (equivalences status)
  } where
    label = max l1 l2
updateStatus _ _ _ = error "too many neighbours in updateStatus"

--updateStatus0 (x , y) status
--  { nextKey  = Extra.next label
--  , mask    = imgSetPixel (x , y) label (mask status)
--  , infoMap  = insert label (addPixel (x , y) mempty) (infoMap status)
--  } where
--    label = nextKey status

newLabellingStatus :: Image a -> LabellingStatus
newLabellingStatus img = LabellingStatus
  { currentCoordinates = (0, 0)
  , nextKey       = 0
  , mask          = copyEmptyImage img
  , infoMap       = mempty
  , equivalences  = mempty
  }

data LabellingStatus = LabellingStatus
  { currentCoordinates :: Coordinates
  , nextKey            :: LabelKey
  , mask               :: Image LabelKey
  , infoMap            :: IntMap LabelInfo
  , equivalences       :: EquivalenceMap
  } deriving stock (Show, Eq, Ord)
