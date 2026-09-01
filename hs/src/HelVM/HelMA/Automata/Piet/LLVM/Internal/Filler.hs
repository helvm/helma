{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Internal.Filler
  ( fillAll
  , paramFilledRefsL
  , paramSourceImageL
  ) where

import           Relude.Extra

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.IntMap                                 as IM
import           Data.Vector                                 ( Vector )
import qualified Data.Vector.Generic                         as V
import           Data.Vector.Mutable                         ( STVector )
import qualified Data.Vector.Mutable                         as VM
import qualified ListT                                       as L

-- TYPES & LENSES

data FillerParams a b s
  = FillerParams
      { paramSourceImage :: Vector (Vector a)
      , paramFilledRefs  :: Vector (STVector s (Maybe b))
      }

paramSourceImageL ∷ Lens' (FillerParams a b s) (Vector (Vector a))
paramSourceImageL = lens paramSourceImage updateSourceImage

paramFilledRefsL ∷ Lens' (FillerParams a b s) (Vector (STVector s (Maybe b)))
paramFilledRefsL = lens paramFilledRefs updateFilledRefs

-- PUBLIC API

fillAll ∷ Eq a ⇒ Vector (Vector a) → (Vector (Vector Int), IntMap [Coordinates])
fillAll image = runST $
  thawImage image >>= processWithThawed image

-- FILLER LOGIC

fillAllST ∷ ( Eq a
             , PrimMonad m
             , MonadReader (FillerParams a Int (PrimState m)) m
             , MonadState Int m
             )
          ⇒ m (IntMap [Coordinates])
fillAllST = IM.fromList <$> L.toList processListT

processListT ∷ ( Eq a
                , PrimMonad m
                , MonadReader (FillerParams a Int (PrimState m)) m
                , MonadState Int m
                )
             ⇒ L.ListT m (Int, [Coordinates])
processListT =
  (L.fromFoldable . V.indexed =<< lift (asksView paramSourceImageL)) >>= processSourceRow

fill ∷ ( Eq a
        , PrimMonad m
        , MonadReader (FillerParams a b (PrimState m)) m
        )
     ⇒ a
     → b
     → Coordinates
     → m [Coordinates]
fill targetColor fillingColor seed = execStateT (fix (fillStep targetColor fillingColor) seed) []

fillStep ∷ ( Eq a
           , PrimMonad m
           , MonadReader (FillerParams a b (PrimState m)) m
           )
        ⇒ a
        → b
        → (Coordinates → StateT [Coordinates] m ())
        → Coordinates
        → StateT [Coordinates] m ()
fillStep targetColor fillingColor rec coord = void . runMaybeT $
  validateColorAndUnfilled targetColor coord *>
    markAndRecurse fillingColor rec coord

-- SUB-LOGIC HELPERS

processWithThawed ∷ (Eq a, PrimMonad m)
                  ⇒ Vector (Vector a)
                  → Vector (STVector (PrimState m) (Maybe Int))
                  → m (Vector (Vector Int), IntMap [Coordinates])
processWithThawed image refs =
  runFillAllST image refs >>= formatResult refs

formatResult ∷ PrimMonad m
             ⇒ Vector (STVector (PrimState m) (Maybe Int))
             → IntMap [Coordinates]
             → m (Vector (Vector Int), IntMap [Coordinates])
formatResult refs positionTable =
  freezeAndFormat refs >>= makeResultPair positionTable

makeResultPair ∷ Applicative m ⇒ b → a → m (a, b)
makeResultPair positionTable filledImage = pure (filledImage, positionTable)

processSourceRow ∷ ( Eq a
                   , PrimMonad m
                   , MonadReader (FillerParams a Int (PrimState m)) m
                   , MonadState Int m
                   )
                ⇒ (Int, Vector a)
                → L.ListT m (Int, [Coordinates])
processSourceRow (y, sourceRow) =
  (L.fromFoldable $ V.indexed sourceRow) >>= processSourceCell y

processSourceCell ∷ ( Eq a
                    , PrimMonad m
                    , MonadReader (FillerParams a Int (PrimState m)) m
                    , MonadState Int m
                    )
                 ⇒ Int
                 → (Int, a)
                 → L.ListT m (Int, [Coordinates])
processSourceCell y (x, targetColor) = checkUnfilledAndIndex targetColor (x, y)

checkUnfilledAndIndex ∷ ( Eq a
                        , PrimMonad m
                        , MonadReader (FillerParams a Int (PrimState m)) m
                        , MonadState Int m
                        )
                      ⇒ a
                      → Coordinates
                      → L.ListT m (Int, [Coordinates])
checkUnfilledAndIndex targetColor coord =
  lift (asksView paramFilledRefsL) >>= lift . readRefAt coord >>= checkCellState targetColor coord

readRefAt ∷ PrimMonad m
          ⇒ Coordinates
          → Vector (STVector (PrimState m) (Maybe b))
          → m (Maybe b)
readRefAt (x, y) filledRefs = VM.read (filledRefs V.! y) x

checkCellState ∷ ( Eq a
                 , PrimMonad m
                 , MonadReader (FillerParams a Int (PrimState m)) m
                 , MonadState Int m
                 )
              ⇒ a
              → Coordinates
              → Maybe Int
              → L.ListT m (Int, [Coordinates])
checkCellState targetColor coord filledColorMaybe =
  guard (isNothing filledColorMaybe) *> processCell targetColor coord

processCell ∷ ( Eq a
              , PrimMonad m
              , MonadReader (FillerParams a Int (PrimState m)) m
              , MonadState Int m
              )
           ⇒ a
           → Coordinates
           → L.ListT m (Int, [Coordinates])
processCell targetColor coord =
  lift get >>= fillCellWithIndex targetColor coord

fillCellWithIndex ∷ ( Eq a
                    , PrimMonad m
                    , MonadReader (FillerParams a Int (PrimState m)) m
                    , MonadState Int m
                    )
                 ⇒ a
                 → Coordinates
                 → Int
                 → L.ListT m (Int, [Coordinates])
fillCellWithIndex targetColor coord blockIndex =
  lift (fill targetColor blockIndex coord) >>= advanceAndPair blockIndex

advanceAndPair ∷ MonadState Int m ⇒ Int → [Coordinates] → L.ListT m (Int, [Coordinates])
advanceAndPair blockIndex filledPositions =
  lift (modify (+1)) *> pure (blockIndex, filledPositions)

validateColorAndUnfilled ∷ ( Eq a
                           , PrimMonad m
                           , MonadReader (FillerParams a b (PrimState m)) m
                           )
                        ⇒ a
                        → Coordinates
                        → MaybeT (StateT [Coordinates] m) ()
validateColorAndUnfilled targetColor coord =
  lift (asksView paramSourceImageL) >>= validatePixel targetColor coord

validatePixel ∷ ( Eq a
                , PrimMonad m
                , MonadReader (FillerParams a b (PrimState m)) m
                )
             ⇒ a
             → Coordinates
             → Vector (Vector a)
             → MaybeT (StateT [Coordinates] m) ()
validatePixel targetColor (x, y) sourceImage =
  hoistMaybe (lookupPixel sourceImage x y) >>= checkSourceAndTargetRef targetColor (x, y)

checkSourceAndTargetRef ∷ ( Eq a
                          , PrimMonad m
                          , MonadReader (FillerParams a b (PrimState m)) m
                          )
                       ⇒ a
                       → Coordinates
                       → a
                       → MaybeT (StateT [Coordinates] m) ()
checkSourceAndTargetRef targetColor coord sourceColor =
  guard (sourceColor == targetColor) *>
    lift (asksView paramFilledRefsL) >>= lift . lift . readRefAt coord >>= guardUnfilled

guardUnfilled ∷ Monad m ⇒ Maybe b → MaybeT m ()
guardUnfilled filledVal = guard (isNothing filledVal)

markAndRecurse ∷ ( PrimMonad m
                 , MonadReader (FillerParams a b (PrimState m)) m
                 )
              ⇒ b
              → (Coordinates → StateT [Coordinates] m ())
              → Coordinates
              → MaybeT (StateT [Coordinates] m) ()
markAndRecurse fillingColor rec coord =
  lift (asksView paramFilledRefsL) >>= writeAndRecurse fillingColor rec coord

writeAndRecurse ∷ PrimMonad m
               ⇒ b
               → (Coordinates → StateT [Coordinates] m ())
               → Coordinates
               → Vector (STVector (PrimState m) (Maybe b))
               → MaybeT (StateT [Coordinates] m) ()
writeAndRecurse fillingColor rec (x, y) filledRefs =
  modify ((x, y) :) *>
    lift (VM.write (filledRefs V.! y) x (Just fillingColor)) *>
      lift (mapM_ rec (getNeighbors (x, y)))

-- GENERAL HELPERS

thawImage ∷ PrimMonad m ⇒ Vector (Vector a) → m (Vector (STVector (PrimState m) (Maybe b)))
thawImage = V.mapM (V.thaw . (Nothing <$))

runFillAllST ∷ ( Eq a
               , PrimMonad m
               )
            ⇒ Vector (Vector a)
            → Vector (STVector (PrimState m) (Maybe Int))
            → m (IntMap [Coordinates])
runFillAllST image refs = runReaderT (evalStateT fillAllST 0) (makeParams image refs)

freezeAndFormat ∷ PrimMonad m ⇒ Vector (STVector (PrimState m) (Maybe Int)) → m (Vector (Vector Int))
freezeAndFormat refs = fmap (fmap (fromMaybe 0)) <$> mapM V.freeze refs

makeParams ∷ Vector (Vector a) → Vector (STVector s (Maybe b)) → FillerParams a b s
makeParams image refs = FillerParams { paramSourceImage = image, paramFilledRefs = refs }

lookupPixel ∷ Vector (Vector a) → Int → Int → Maybe a
lookupPixel img x y = (V.!? x) =<< img V.!? y

getNeighbors ∷ Coordinates → [Coordinates]
getNeighbors (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

asksView ∷ MonadReader r m ⇒ Lens' r a → m a
asksView l = asks (view l)

updateSourceImage ∷ FillerParams a b s → Vector (Vector a) → FillerParams a b s
updateSourceImage s x = s { paramSourceImage = x }

updateFilledRefs ∷ FillerParams a b s → Vector (STVector s (Maybe b)) → FillerParams a b s
updateFilledRefs s x = s { paramFilledRefs = x }

