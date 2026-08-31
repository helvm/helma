module HelVM.HelMA.Automata.Piet.Compiler
  ( compile
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates ( Coordinates )
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Labelling
import           HelVM.HelMA.Automata.Piet.Types.Matrix
import           HelVM.HelMA.Automata.Piet.Types.Program     ( CodelSize, Program (Program) )

import           Data.IntMap                                 hiding ( filter )
import qualified Data.Map                                    as Map

import           Lens.Micro.Platform

import qualified Relude.Extra                                as Extra

-- TYPES & LENSES

type EquivalenceMap = IntMap LabelKey

data LabellingStatus
  = LabellingStatus
      { _currentCoords :: Coordinates
      , _nextKey       :: LabelKey
      , _labelling     :: Labelling
      , _equivalences  :: EquivalenceMap
      }
  deriving stock (Show)

makeLenses ''LabellingStatus

-- PUBLIC API

compile ∷ CodelSize → Matrix Color → Program
compile cs img = Program cs img (label4 img)

-- COMPILER CORE (LABELING PROCESS)

label4 ∷ Eq a ⇒ Matrix a → Labelling
label4 = label4With (==)

label4With ∷ (a → a → Bool) → Matrix a → Labelling
label4With neighbours img = Labelling img' inf where
  (status, assocMap) = label4With' neighbours img (LabellingStatus (0, 0) 0 (Labelling (newMatrix (0,0) []) mempty) mempty) Map.empty
  currentLabelling   = status ^. labelling

  maskImg = newMatrix (widthMatrix img, heightMatrix img) (Map.toList assocMap)

  img' = fmap (applyEquivClass (status ^. equivalences)) maskImg
  inf  = foldrWithKey (mergeClass (status ^. equivalences)) mempty (currentLabelling ^. info)

  applyEquivClass eqMap lbl = equivClass lbl eqMap
  mergeClass eqMap label labelInfo = alter (updateMap labelInfo) (equivClass label eqMap)

label4With' ∷ (a → a → Bool) → Matrix a → LabellingStatus → Map.Map Coordinates LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
label4With' neighbours img status acc = checkNext (nextCoords (widthMatrix img, heightMatrix img) xy) neighbours img (updateStatus mergeLabels status acc xy) where
  xy@(x, y) = status ^. currentCoords
  pixel     = pixelMatrix (x, y) img

  mergeLabels = fmap getMaskLabel $ filter isNeighbour $ addPixelInfo <$> previousNeighbours xy

  addPixelInfo (nx, ny) = (nx, ny, pixelMatrix (nx, ny) img)
  isNeighbour (_, _, e) = neighbours pixel e
  getMaskLabel (nx, ny, _) = Map.findWithDefault (error "Missing label") (nx, ny) acc

  previousNeighbours (cx, cy) = filter validCoord [ (cx-1, cy), (cx, cy-1) ]
  validCoord (nx, ny) = nx >= 0 && ny >= 0

checkNext ∷ Maybe Coordinates → (a → a → Bool) → Matrix a → (LabellingStatus, Map.Map Coordinates LabelKey) → (LabellingStatus, Map.Map Coordinates LabelKey)
checkNext Nothing    _          _   res       = res
checkNext (Just xy') neighbours img (s, acc') = label4With' neighbours img (s & currentCoords .~ xy') acc'

-- STATUS & MAP UPDATES

updateStatus ∷ [LabelKey] → LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → (LabellingStatus, Map.Map Coordinates LabelKey)
updateStatus []       status acc xy = updateEmptyStatus status acc xy
updateStatus [l]      status acc xy = updateSingleStatus status acc xy l
updateStatus [l1, l2] status acc xy = updatePairStatus status acc xy l1 l2
updateStatus _        _      _   _  = error "too many neighbours in HelVM.HelMA.Automata.Piet.Compiler.ImageProcessor.label4With'"

updateEmptyStatus ∷ LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → (LabellingStatus, Map.Map Coordinates LabelKey)
updateEmptyStatus status acc xy = setLabel status' acc xy (status ^. nextKey) where
  status' = status
    & (labelling . info %~ insert (status ^. nextKey) (addPixel xy Nothing))
    & (nextKey %~ Extra.next)

updateSingleStatus ∷ LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
updateSingleStatus status acc xy l = setLabel status' acc xy l where
  status' = status & (labelling . info %~ adjust (addPixel xy) l)

updatePairStatus ∷ LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → LabelKey → LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
updatePairStatus status acc xy l1 l2 = setLabel status' acc xy targetLabel where
  targetLabel = max l1 l2
  status'     = status
    & (labelling . info %~ adjust (addPixel xy) targetLabel)
    & (equivalences %~ equivInsert l1 l2)

setLabel ∷ LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
setLabel status acc xy label = (status, Map.insert xy label acc)

updateMap ∷ Maybe LabelInfo → Maybe (Maybe LabelInfo) → Maybe (Maybe LabelInfo)
updateMap Nothing    Nothing           = Nothing
updateMap (Just new) Nothing           = Just (Just new)
updateMap Nothing    (Just old)        = Just old
updateMap (Just new) (Just Nothing)    = Just (Just new)
updateMap (Just new) (Just (Just old)) = Just (Just (new <> old))

-- EQUIVALENCE CLASS UTILS

equivClass ∷ LabelKey → EquivalenceMap → LabelKey
equivClass e = findWithDefault e e

equivInsert ∷ LabelKey → LabelKey → EquivalenceMap → EquivalenceMap
equivInsert x y = guardInsert (x /= y) x y

guardInsert ∷ Bool → LabelKey → LabelKey → EquivalenceMap → EquivalenceMap
guardInsert False _ _ mp = mp
guardInsert True  x y mp = fmap (replaceClass newClass classes) $ insert x newClass $ insert y newClass mp where
  class1   = equivClass x mp
  class2   = equivClass y mp
  classes  = x :| [y, class1, class2]
  newClass = Extra.minimum1 classes

replaceClass ∷ LabelKey → NonEmpty LabelKey → LabelKey → LabelKey
replaceClass newClass classes eqClass = checkInClass (eqClass `elem` classes) newClass eqClass

checkInClass ∷ Bool → LabelKey → LabelKey → LabelKey
checkInClass False _        eqc = eqc
checkInClass True  newClass _   = newClass

-- COORDINATES TRAVERSAL UTILS

nextCoords ∷ Coordinates → Coordinates → Maybe Coordinates
nextCoords (w, h) (cx, cy) = guardX (cx < w - 1) cx cy h

guardX ∷ Bool → Int → Int → Int → Maybe Coordinates
guardX False _  cy h = guardY (cy < h - 1) cy
guardX True  cx cy _ = Just (cx + 1, cy)

guardY ∷ Bool → Int → Maybe Coordinates
guardY False _  = Nothing
guardY True  cy = Just (0, cy + 1)
