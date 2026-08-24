module HelVM.HelMA.Automata.Piet.Compiler
  ( compile
  , label4
  , label4With
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates ( Coordinates )
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Labelling
import           HelVM.HelMA.Automata.Piet.Types.Program     ( CodelSize, Program (Program) )

import           Data.IntMap                                 hiding ( filter )
import qualified Data.Map                                    as Map

import           Lens.Micro.Platform

import qualified Relude.Extra                                as Extra

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

compile ∷ CodelSize → Image Color → Program
compile cs img = Program cs img (label4 img)

label4 ∷ Eq a ⇒ Image a → Labelling
label4 = label4With (==)

label4With ∷ (a → a → Bool) → Image a → Labelling
label4With neighbours img = Labelling img' inf where
  (status, assocMap) = label4With' neighbours img (LabellingStatus (0, 0) 0 (Labelling (newImage (0,0) []) mempty) mempty) Map.empty
  currentLabelling   = status ^. labelling

  maskImg = newImage (widthImage img, heightImage img) (Map.toList assocMap)

  img' = fmap (applyEquivClass (status ^. equivalences)) maskImg
  inf  = foldrWithKey (mergeClass (status ^. equivalences)) mempty (currentLabelling ^. info)

  applyEquivClass eqMap lbl = equivClass lbl eqMap

  mergeClass eqMap label labelInfo = alter (updateMap labelInfo) (equivClass label eqMap)

  updateMap Nothing    Nothing            = Nothing
  updateMap (Just new) Nothing            = Just (Just new)
  updateMap Nothing    (Just old)         = Just old
  updateMap (Just new) (Just (Just oldS)) = Just (Just (new <> oldS))
  updateMap _          _                  = Nothing

label4With' ∷ (a → a → Bool) → Image a → LabellingStatus → Map.Map Coordinates LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
label4With' neighbours img status acc = checkNext (nextCoords xy) (updateStatus mergeLabels) where
  xy@(x, y) = status ^. currentCoords
  pixel     = pixelImage (x, y) img

  mergeLabels = fmap getMaskLabel $ filter isNeighbour $ addPixelInfo <$> previousNeighbours xy

  addPixelInfo (nx, ny) = (nx, ny, pixelImage (nx, ny) img)
  isNeighbour (_, _, e) = neighbours pixel e
  getMaskLabel (nx, ny, _) = Map.findWithDefault (error "Missing label") (nx, ny) acc

  updateStatus []       = updateEmptyStatus status acc (x, y)
  updateStatus [l]      = updateSingleStatus status acc (x, y) l
  updateStatus [l1, l2] = updatePairStatus status acc (x, y) l1 l2
  updateStatus _        = error "too many neighbours in HelVM.HelMA.Automata.Piet.Compiler.ImageProcessor.label4With'"

  checkNext (Just xy') (s, acc') = label4With' neighbours img (s & currentCoords .~ xy') acc'
  checkNext Nothing    res       = res

  previousNeighbours (cx, cy) = filter validCoord [ (cx-1, cy), (cx, cy-1) ]
  validCoord (nx, ny) = nx >= 0 && ny >= 0

  nextCoords (cx, cy) = guardX (cx < widthImage img - 1) cx cy
  guardX True  cx cy = Just (cx + 1, cy)
  guardX False _ cy  = guardY (cy < heightImage img - 1) cy
  guardY True  cy = Just (0, cy + 1)
  guardY False _  = Nothing

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
