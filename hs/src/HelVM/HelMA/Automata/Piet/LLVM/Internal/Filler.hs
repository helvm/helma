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
fillAll image = runST $ do
  filledRefs <- V.mapM (V.thaw . (Nothing <$)) image
  let params = FillerParams { paramSourceImage = image, paramFilledRefs = filledRefs }
  positionTable <- runReaderT (evalStateT fillAllST 0) params
  filledImageMaybe <- mapM V.freeze filledRefs
  let filledImage = fmap (fromMaybe 0) <$> filledImageMaybe
  pure (filledImage, positionTable)

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
processListT = do
  sourceImage <- lift $ asksView paramSourceImageL
  (y, sourceRow) <- L.fromFoldable $ V.indexed sourceImage
  (x, targetColor) <- L.fromFoldable $ V.indexed sourceRow

  filledRefs <- lift $ asksView paramFilledRefsL
  filledColorMaybe <- lift $ VM.read (filledRefs V.! y) x
  guard $ isNothing filledColorMaybe

  blockIndex <- lift get
  filledPositions <- lift $ fill targetColor blockIndex (x, y)
  lift $ modify (+1)

  pure (blockIndex, filledPositions)

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
fillStep targetColor fillingColor rec (x, y) = void . runMaybeT $ do
  sourceImage <- lift $ asksView paramSourceImageL
  sourceColor <- hoistMaybe $ lookupPixel sourceImage x y
  guard $ sourceColor == targetColor

  filledRefs <- lift $ asksView paramFilledRefsL
  let filledRow = filledRefs V.! y
  filledVal <- lift $ VM.read filledRow x
  guard $ isNothing filledVal

  modify ((x, y) :)
  lift $ VM.write filledRow x (Just fillingColor)
  lift $ mapM_ rec (getNeighbors (x, y))

-- HELPERS

lookupPixel ∷ Vector (Vector a) → Int → Int → Maybe a
lookupPixel img x y = img V.!? y >>= (V.!? x)

getNeighbors ∷ Coordinates → [Coordinates]
getNeighbors (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

asksView ∷ MonadReader r m ⇒ Lens' r a → m a
asksView l = asks (view l)

updateSourceImage ∷ FillerParams a b s → Vector (Vector a) → FillerParams a b s
updateSourceImage s x = s { paramSourceImage = x }

updateFilledRefs ∷ FillerParams a b s → Vector (STVector s (Maybe b)) → FillerParams a b s
updateFilledRefs s x = s { paramFilledRefs = x }
