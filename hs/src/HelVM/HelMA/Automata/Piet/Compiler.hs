module HelVM.HelMA.Automata.Piet.Compiler
  ( compile
  , label4
  , label4With
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Program


import           Data.IntMap                                 hiding (filter)

import qualified Relude.Extra                                as Extra

compile ∷ Image Color → Program
compile image_ = Program image_ mask__ info_ where
  (mask__, info_) = label4 image_

data LabellingStatus = LabellingStatus
  { _currentCoords :: Coordinates
  , _nextKey       :: LabelKey
  , _mask          :: Image LabelKey
  , _infoMap       :: IntMap (Maybe LabelInfo)
  , _equivalences  :: EquivalenceMap
  } deriving stock (Show)

label4 ∷ Eq a ⇒ Image a → (Image LabelKey, IntMap (Maybe LabelInfo))
label4 = label4With (==)

label4With ∷ (a → a → Bool) → Image a → (Image LabelKey, IntMap (Maybe LabelInfo))
label4With neighbours img = (img', inf) where
  status = label4With' neighbours img (LabellingStatus (0, 0) 0 (newImage (witdthImage img, heightImage img) []) mempty mempty)
  img'   = fmap (applyEquivClass (_equivalences status)) (_mask status)
  inf    = foldrWithKey (mergeClass (_equivalences status)) mempty (_infoMap status)

  applyEquivClass eqMap lbl = equivClass lbl eqMap

  mergeClass eqMap label labelInfo = alter (updateMap labelInfo) (equivClass label eqMap)

  updateMap Nothing    Nothing            = Nothing
  updateMap (Just new) Nothing            = Just (Just new)
  updateMap Nothing    (Just old)         = Just old
  updateMap (Just new) (Just (Just oldS)) = Just (Just (new <> oldS))
  updateMap _          _                  = Nothing

label4With' ∷ (a → a → Bool) → Image a → LabellingStatus → LabellingStatus
label4With' neighbours img status = checkNext (nextCoords xy) (updateStatus mergeLabels) where
  xy@(x, y) = _currentCoords status
  pixel     = pixelImage (x, y) img

  mergeLabels = fmap getMaskLabel $ filter isNeighbour $ addPixelInfo <$> previousNeighbours xy

  addPixelInfo (nx, ny) = (nx, ny, pixelImage (nx, ny) img)
  isNeighbour (_, _, e) = neighbours pixel e
  getMaskLabel (nx, ny, _) = pixelImage (nx, ny) (_mask status)

  updateStatus []       = status { _nextKey = Extra.next (_nextKey status), _mask = setPixelImage (x, y) (_nextKey status) (_mask status), _infoMap = insert (_nextKey status) (addPixel (x, y) Nothing) (_infoMap status) }
  updateStatus [l]      = status { _mask = setPixelImage (x, y) l (_mask status), _infoMap = adjust (addPixel (x, y)) l (_infoMap status) }
  updateStatus [l1, l2] = status { _mask = setPixelImage (x, y) (max l1 l2) (_mask status), _infoMap = adjust (addPixel (x, y)) (max l1 l2) (_infoMap status), _equivalences = equivInsert l1 l2 (_equivalences status) }
  updateStatus _        = error "too many neighbours in HelVM.HelMA.Automata.Piet.Compiler.ImageProcessor.label4With'"
  checkNext (Just xy') s = label4With' neighbours img (s { _currentCoords = xy' })
  checkNext Nothing    s = s

  previousNeighbours (cx, cy) = filter validCoord [ (cx-1, cy), (cx, cy-1) ]
  validCoord (nx, ny) = nx >= 0 && ny >= 0

  nextCoords (cx, cy) = guardX (cx < witdthImage img - 1) cx cy
  guardX True  cx cy = Just (cx + 1, cy)
  guardX False _ cy  = guardY (cy < heightImage img - 1) cy
  guardY True  cy = Just (0, cy + 1)
  guardY False _  = Nothing

type EquivalenceMap = IntMap LabelKey

equivClass ∷ LabelKey → EquivalenceMap → LabelKey
equivClass e = findWithDefault e e

equivInsert ∷ LabelKey → LabelKey → EquivalenceMap → EquivalenceMap
equivInsert x y mp = guardInsert (x /= y) where
  guardInsert True  = fmap replaceClass $ insert x newClass $ insert y newClass mp
  guardInsert False = mp

  class1   = equivClass x mp
  class2   = equivClass y mp
  classes  = x :| [y, class1, class2]
  newClass = Extra.minimum1 classes

  replaceClass eqClass = checkInClass (eqClass `elem` classes) eqClass
  checkInClass True  _   = newClass
  checkInClass False eqc = eqc
