module HelVM.HelMA.Automata.Piet.LabellingStatus where

import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.EquivalenceMap
import           HelVM.HelMA.Automata.Piet.FullInfo
import           HelVM.HelMA.Automata.Piet.Image

import           Data.Function.Tools
import           Data.IntMap                              (adjust, insert)

import qualified Relude.Extra                             as Extra

updateStatus :: [LabelKey]  -> Coordinates -> LabellingStatus -> LabellingStatus
updateStatus []       = updateStatus0
updateStatus [label]  = updateStatus1 label
updateStatus [l1, l2] = updateStatus2 l1 l2
updateStatus _        = const2 (error "too many neighbours in updateStatus")

updateStatus0 :: Coordinates -> LabellingStatus -> LabellingStatus
updateStatus0 (x , y) status = status
  { nextKey  = Extra.next label
  , mask     = imgSetPixel (x , y) label (mask status)
  , infoMap  = insert label (addPixel (x , y) mempty) (infoMap status)
  } where
    label = nextKey status

updateStatus1 :: LabelKey -> Coordinates -> LabellingStatus -> LabellingStatus
updateStatus1 label (x , y) status = status
  { mask     = imgSetPixel (x , y) label (mask status)
  , infoMap  = adjust (addPixel (x , y)) label (infoMap status)
  }

updateStatus2 :: LabelKey -> LabelKey -> Coordinates -> LabellingStatus -> LabellingStatus
updateStatus2 l1 l2 (x , y) status = status
  { mask          = imgSetPixel (x , y) label (mask status)
  , infoMap       = adjust (addPixel (x, y)) label (infoMap status)
  , equivalences  = equivInsert l1 l2 (equivalences status)
  } where
    label = max l1 l2

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
