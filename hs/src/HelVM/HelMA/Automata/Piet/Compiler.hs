{-# LANGUAGE TemplateHaskell #-}
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
import           HelVM.HelMA.Automata.Piet.Types.Program     ( Program (Program) )

import           Data.IntMap                                 hiding ( filter )

import           Lens.Micro
import           Lens.Micro.TH                               ( makeLenses )

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

compile ∷ Image Color → Program
compile image_ = Program image_ (label4 image_)

label4 ∷ Eq a ⇒ Image a → Labelling
label4 = label4With (==)

label4With ∷ (a → a → Bool) → Image a → Labelling
label4With neighbours img = Labelling img' inf where
  initialLabelling = Labelling (newImage (witdthImage img, heightImage img) []) mempty
  status           = label4With' neighbours img (LabellingStatus (0, 0) 0 initialLabelling mempty)
  currentLabelling = status ^. labelling

  img' = fmap (applyEquivClass (status ^. equivalences)) (currentLabelling ^. mask)
  inf  = foldrWithKey (mergeClass (status ^. equivalences)) mempty (currentLabelling ^. info)

  applyEquivClass eqMap lbl = equivClass lbl eqMap

  mergeClass eqMap label labelInfo = alter (updateMap labelInfo) (equivClass label eqMap)

  updateMap Nothing    Nothing            = Nothing
  updateMap (Just new) Nothing            = Just (Just new)
  updateMap Nothing    (Just old)         = Just old
  updateMap (Just new) (Just (Just oldS)) = Just (Just (new <> oldS))
  updateMap _          _                  = Nothing

label4With' ∷ (a → a → Bool) → Image a → LabellingStatus → LabellingStatus
label4With' neighbours img status = checkNext (nextCoords xy) (updateStatus mergeLabels) where
  xy@(x, y) = status ^. currentCoords
  pixel     = pixelImage (x, y) img
  lblng     = status ^. labelling

  mergeLabels = fmap getMaskLabel $ filter isNeighbour $ addPixelInfo <$> previousNeighbours xy

  addPixelInfo (nx, ny) = (nx, ny, pixelImage (nx, ny) img)
  isNeighbour (_, _, e) = neighbours pixel e
  getMaskLabel (nx, ny, _) = pixelImage (nx, ny) (lblng ^. mask)

  updateStatus [] = status
    & (labelling . mask %~ setPixelImage (x, y) (status ^. nextKey))
    & (labelling . info %~ insert (status ^. nextKey) (addPixel (x, y) Nothing))
    & (nextKey %~ Extra.next)

  updateStatus [l] = status
    & (labelling . mask %~ setPixelImage (x, y) l)
    & (labelling . info %~ adjust (addPixel (x, y)) l)

  updateStatus [l1, l2] = status
    & (labelling . mask %~ setPixelImage (x, y) (max l1 l2))
    & (labelling . info %~ adjust (addPixel (x, y)) (max l1 l2))
    & (equivalences %~ equivInsert l1 l2)

  updateStatus _ = error "too many neighbours in HelVM.HelMA.Automata.Piet.Compiler.ImageProcessor.label4With'"

  checkNext (Just xy') s = label4With' neighbours img (s & currentCoords .~ xy')
  checkNext Nothing    s = s

  previousNeighbours (cx, cy) = filter validCoord [ (cx-1, cy), (cx, cy-1) ]
  validCoord (nx, ny) = nx >= 0 && ny >= 0

  nextCoords (cx, cy) = guardX (cx < witdthImage img - 1) cx cy
  guardX True  cx cy = Just (cx + 1, cy)
  guardX False _ cy  = guardY (cy < heightImage img - 1) cy
  guardY True  cy = Just (0, cy + 1)
  guardY False _  = Nothing

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
