module HelVM.HelMA.Automata.Piet.Compiler
  ( compile
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates ( Coordinates )
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Labelling
import           HelVM.HelMA.Automata.Piet.Types.Program     ( CodelSize, Program (Program) )

import qualified Data.IntMap                                 as IntMap
import qualified Data.Map                                    as Map

import           Relude.Extra

-- TYPES & LENSES

type EquivalenceMap = IntMap LabelKey

data LabellingStatus
  = LabellingStatus
      { currentCoords :: Coordinates
      , nextKey       :: LabelKey
      , labelling     :: Labelling
      , equivalences  :: EquivalenceMap
      }
  deriving stock (Show)

currentCoordsL ∷ Lens' LabellingStatus Coordinates
currentCoordsL = lens currentCoords (\s x -> s { currentCoords = x })

nextKeyL ∷ Lens' LabellingStatus LabelKey
nextKeyL = lens nextKey (\s x -> s { nextKey = x })

labellingL ∷ Lens' LabellingStatus Labelling
labellingL = lens labelling (\s x -> s { labelling = x })

equivalencesL ∷ Lens' LabellingStatus EquivalenceMap
equivalencesL = lens equivalences (\s x -> s { equivalences = x })

-- PUBLIC API

compile ∷ CodelSize → Grid Color → Program
compile cs img = Program cs img (label4 img)

-- COMPILER CORE (LABELING PROCESS)

label4 ∷ Eq a ⇒ Grid a → Labelling
label4 = label4With (==)

label4With ∷ (a → a → Bool) → Grid a → Labelling
label4With neighbours img = Labelling img' inf where
  (status, assocMap) = label4With' neighbours img (LabellingStatus (0, 0) 0 (Labelling (newGrid (0,0) []) mempty) mempty) Map.empty
  currentLabelling   = status ^. labellingL

  maskImg = newGrid (widthGrid img, heightGrid img) (Map.toList assocMap)

  img' = fmap (applyEquivClass (status ^. equivalencesL)) maskImg
  inf  = IntMap.foldrWithKey (mergeClass (status ^. equivalencesL)) mempty (currentLabelling ^. infoL)

  applyEquivClass eqMap lbl = equivClass lbl eqMap
  mergeClass eqMap label labelInfo = alter (updateMap labelInfo) (equivClass label eqMap)

label4With' ∷ (a → a → Bool) → Grid a → LabellingStatus → Map.Map Coordinates LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
label4With' neighbours img status acc = checkNext (nextCoords img xy) neighbours img (updateStatus mergeLabels status acc xy) where
  pixel  = atGrid xy img
  xy     = status ^. currentCoordsL

  mergeLabels = fmap getMaskLabel $ filter isNeighbour $ addPixelInfo <$> previousNeighbours xy

  addPixelInfo (nx, ny) = (nx, ny, atGrid (nx, ny) img)
  isNeighbour (_, _, e) = neighbours pixel e
  getMaskLabel (nx, ny, _) = Map.findWithDefault (error "Missing label") (nx, ny) acc

  previousNeighbours (cx, cy) = filter validCoord [ (cx-1, cy), (cx, cy-1) ]
  validCoord (nx, ny) = nx >= 0 && ny >= 0

checkNext ∷ Maybe Coordinates → (a → a → Bool) → Grid a → (LabellingStatus, Map.Map Coordinates LabelKey) → (LabellingStatus, Map.Map Coordinates LabelKey)
checkNext Nothing    _          _   res      = res
checkNext (Just xy) neighbours img (s, acc') = label4With' neighbours img (s & currentCoordsL .~ xy) acc'

-- STATUS & MAP UPDATES

updateStatus ∷ [LabelKey] → LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → (LabellingStatus, Map.Map Coordinates LabelKey)
updateStatus []       status acc xy = updateEmptyStatus status acc xy
updateStatus [l]      status acc xy = updateSingleStatus status acc xy l
updateStatus [l1, l2] status acc xy = updatePairStatus status acc xy l1 l2
updateStatus _        _      _   _  = error "too many neighbours in HelVM.HelMA.Automata.Piet.Compiler.ImageProcessor.label4With'"

updateEmptyStatus ∷ LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → (LabellingStatus, Map.Map Coordinates LabelKey)
updateEmptyStatus status acc xy = setLabel status' acc xy (status ^. nextKeyL) where
  status' = status
    & (labellingL . infoL %~ insert (status ^. nextKeyL) (addPixel xy Nothing))
    & (nextKeyL %~ next)

updateSingleStatus ∷ LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
updateSingleStatus status acc xy l = setLabel status' acc xy l where
  status' = status & (labellingL . infoL %~ IntMap.adjust (addPixel xy) l)

updatePairStatus ∷ LabellingStatus → Map.Map Coordinates LabelKey → Coordinates → LabelKey → LabelKey → (LabellingStatus, Map.Map Coordinates LabelKey)
updatePairStatus status acc xy l1 l2 = setLabel status' acc xy targetLabel where
  targetLabel = max l1 l2
  status'     = status
    & (labellingL . infoL %~ IntMap.adjust (addPixel xy) targetLabel)
    & (equivalencesL %~ equivInsert l1 l2)

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
equivClass e = IntMap.findWithDefault e e

equivInsert ∷ LabelKey → LabelKey → EquivalenceMap → EquivalenceMap
equivInsert x y = guardInsert (x /= y) x y

guardInsert ∷ Bool → LabelKey → LabelKey → EquivalenceMap → EquivalenceMap
guardInsert False _ _ mp = mp
guardInsert True  x y mp = fmap (replaceClass newClass classes) $ insert x newClass $ insert y newClass mp where
  class1   = equivClass x mp
  class2   = equivClass y mp
  classes  = x :| [y, class1, class2]
  newClass = minimum1 classes

replaceClass ∷ LabelKey → NonEmpty LabelKey → LabelKey → LabelKey
replaceClass newClass classes eqClass = checkInClass (eqClass `elem` classes) newClass eqClass

checkInClass ∷ Bool → LabelKey → LabelKey → LabelKey
checkInClass False _        eqc = eqc
checkInClass True  newClass _   = newClass
