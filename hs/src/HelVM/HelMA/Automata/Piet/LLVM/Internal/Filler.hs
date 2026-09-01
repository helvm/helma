{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Internal.Filler
  ( fillAll
  , paramFilledRefsL
  , paramSourceImageL
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Control.Monad.Primitive
import           Control.Monad.ST

import qualified Data.IntMap                                 as IM
import           Data.Vector                                 ( Vector )
import qualified Data.Vector.Generic                         as V
import           Data.Vector.Mutable                         ( STVector )
import qualified Data.Vector.Mutable                         as VM

import qualified ListT                                       as L

import           Relude.Extra

-- TYPES & ALIASES

type Matrix a = Vector (Vector a)
type STMatrix s b = Vector (STVector s (Maybe b))

type FillMonad a b m =
  ( Eq a
  , PrimMonad m
  , MonadReader (FillerParams a b (PrimState m)) m
  )

type FillStateMonad a m =
  ( FillMonad a Int m
  , MonadState Int m
  )

type ListMonad m a = L.ListT m a

type FillStepMonad m a = MaybeT (StateT BlockCoordinates m) a

type StepRec m = Coordinates → StateT BlockCoordinates m ()

-- DATA & LENSES

data FillerParams a b s
  = FillerParams
      { paramSourceImage :: Matrix a
      , paramFilledRefs  :: STMatrix s b
      }

paramSourceImageL ∷ Lens' (FillerParams a b s) (Matrix a)
paramSourceImageL = lens paramSourceImage updateSourceImage

paramFilledRefsL ∷ Lens' (FillerParams a b s) (STMatrix s b)
paramFilledRefsL = lens paramFilledRefs updateFilledRefs

-- PUBLIC API

fillAll ∷ Eq a ⇒ Matrix a → (Matrix Int, IntMap BlockCoordinates)
fillAll image = runST $ processWithThawed image =<< thawImage image

-- FILLER LOGIC

fillAllST ∷ FillStateMonad a m ⇒ m (IntMap BlockCoordinates)
fillAllST = IM.fromList <$> L.toList processListT

processListT ∷ FillStateMonad a m ⇒ ListMonad m (Int, BlockCoordinates)
processListT = processSourceRow =<< (L.fromFoldable . V.indexed =<< lift (asksView paramSourceImageL))

fill ∷ FillMonad a b m ⇒ a → b → Coordinates → m BlockCoordinates
fill targetColor fillingColor seed = execStateT (fix (fillStep targetColor fillingColor) seed) []

fillStep ∷ FillMonad a b m ⇒ a → b → StepRec m → Coordinates → StateT BlockCoordinates m ()
fillStep targetColor fillingColor rec coord = void . runMaybeT $ validateColorAndUnfilled targetColor coord *> markAndRecurse fillingColor rec coord

-- SUB-LOGIC HELPERS

processWithThawed ∷ (Eq a, PrimMonad m) ⇒ Matrix a → STMatrix (PrimState m) Int → m (Matrix Int, IntMap BlockCoordinates)
processWithThawed image refs = formatResult refs =<< runFillAllST image refs

formatResult ∷ PrimMonad m ⇒ STMatrix (PrimState m) Int→ IntMap BlockCoordinates → m (Matrix Int, IntMap BlockCoordinates)
formatResult refs positionTable = makeResultPair positionTable =<< freezeAndFormat refs

makeResultPair ∷ Applicative m ⇒ b → a → m (a, b)
makeResultPair positionTable filledImage = pure (filledImage, positionTable)

processSourceRow ∷ FillStateMonad a m ⇒ (Int, Vector a) → ListMonad m (Int, BlockCoordinates)
processSourceRow (y, sourceRow) = processSourceCell y =<< L.fromFoldable (V.indexed sourceRow)

processSourceCell ∷ FillStateMonad a m ⇒ Int → (Int, a) → ListMonad m (Int, BlockCoordinates)
processSourceCell y (x, targetColor) = checkUnfilledAndIndex targetColor (x, y)

checkUnfilledAndIndex ∷ FillStateMonad a m ⇒ a → Coordinates → ListMonad m (Int, BlockCoordinates)
checkUnfilledAndIndex targetColor coord =
  checkCellState targetColor coord =<< (lift . readRefAt coord =<< lift (asksView paramFilledRefsL))

readRefAt ∷ PrimMonad m ⇒ Coordinates → STMatrix (PrimState m) b → m (Maybe b)
readRefAt (x, y) filledRefs = VM.read (filledRefs V.! y) x

checkCellState ∷ FillStateMonad a m ⇒ a → Coordinates → Maybe Int → ListMonad m (Int, BlockCoordinates)
checkCellState targetColor coord filledColorMaybe =
  guard (isNothing filledColorMaybe) *> processCell targetColor coord

processCell ∷ FillStateMonad a m ⇒ a → Coordinates → ListMonad m (Int, BlockCoordinates)
processCell targetColor coord =
  fillCellWithIndex targetColor coord =<< lift get

fillCellWithIndex ∷ FillStateMonad a m ⇒ a → Coordinates → Int → ListMonad m (Int, BlockCoordinates)
fillCellWithIndex targetColor coord blockIndex =
  advanceAndPair blockIndex =<< lift (fill targetColor blockIndex coord)

advanceAndPair ∷ MonadState Int m ⇒ Int → BlockCoordinates → ListMonad m (Int, BlockCoordinates)
advanceAndPair blockIndex filledPositions = lift (modify (+1)) $> (blockIndex, filledPositions)

validateColorAndUnfilled ∷ FillMonad a b m ⇒ a → Coordinates → FillStepMonad m ()
validateColorAndUnfilled targetColor coord = validatePixel targetColor coord =<< lift (asksView paramSourceImageL)

validatePixel ∷ FillMonad a b m ⇒ a → Coordinates → Matrix a → FillStepMonad m ()
validatePixel targetColor p sourceImage = checkSourceAndTargetRef targetColor p =<< hoistMaybe (lookupPixel sourceImage p)

checkSourceAndTargetRef ∷ FillMonad a b m ⇒ a → Coordinates → a → FillStepMonad m ()
checkSourceAndTargetRef targetColor coord sourceColor = guard (sourceColor == targetColor) *> (guardUnfilled =<< (lift . lift . readRefAt coord =<< lift (asksView paramFilledRefsL)))

guardUnfilled ∷ Monad m ⇒ Maybe b → MaybeT m ()
guardUnfilled filledVal = guard (isNothing filledVal)

markAndRecurse ∷ FillMonad a b m ⇒ b → StepRec m → Coordinates → FillStepMonad m ()
markAndRecurse fillingColor rec coord = writeAndRecurse fillingColor rec coord =<< lift (asksView paramFilledRefsL)

writeAndRecurse ∷ PrimMonad m ⇒ b → StepRec m → Coordinates → STMatrix (PrimState m) b → FillStepMonad m ()
writeAndRecurse fillingColor rec (x, y) filledRefs = modify ((x, y) :) *> lift (VM.write (filledRefs V.! y) x (Just fillingColor)) *> lift (mapM_ rec (getNeighbors (x, y)))

-- GENERAL HELPERS

thawImage ∷ PrimMonad m ⇒ Matrix a → m (STMatrix (PrimState m) b)
thawImage = V.mapM (V.thaw . (Nothing <$))

runFillAllST ∷ (Eq a, PrimMonad m) ⇒ Matrix a → STMatrix (PrimState m) Int → m (IntMap BlockCoordinates)
runFillAllST image refs = runReaderT (evalStateT fillAllST 0) (makeParams image refs)

freezeAndFormat ∷ PrimMonad m ⇒ STMatrix (PrimState m) Int → m (Matrix Int)
freezeAndFormat refs = fmap (fmap (fromMaybe 0)) <$> mapM V.freeze refs

makeParams ∷ Matrix a → STMatrix s b → FillerParams a b s
makeParams image refs = FillerParams { paramSourceImage = image, paramFilledRefs = refs }

lookupPixel ∷ Matrix a → Coordinates → Maybe a
lookupPixel img (x, y) = (V.!? x) =<< img V.!? y

getNeighbors ∷ Coordinates → BlockCoordinates
getNeighbors (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

asksView ∷ MonadReader r m ⇒ Lens' r a → m a
asksView l = asks (view l)

updateSourceImage ∷ FillerParams a b s → Matrix a → FillerParams a b s
updateSourceImage s x = s { paramSourceImage = x }

updateFilledRefs ∷ FillerParams a b s → STMatrix s b → FillerParams a b s
updateFilledRefs s x = s { paramFilledRefs = x }
