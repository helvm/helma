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

-- TYPES & LENSES

data FillerParams a b s
  = FillerParams
      { paramSourceImage :: Vector (Vector a)
      , paramFilledRefs  :: Vector (STVector s (Maybe b))
      }

paramSourceImageL ∷ Lens' (FillerParams a b s) (Vector (Vector a))
paramSourceImageL = lens paramSourceImage (\s x -> s { paramSourceImage = x })

paramFilledRefsL ∷ Lens' (FillerParams a b s) (Vector (STVector s (Maybe b)))
paramFilledRefsL = lens paramFilledRefs (\s x -> s { paramFilledRefs = x })

-- HELPER FOR MONADREADER

asksView ∷ MonadReader r m ⇒ Lens' r a → m a
asksView l = asks (view l)

-- PUBLIC API

fillAll ∷ Eq a ⇒ Vector (Vector a) → (Vector (Vector Int), IntMap [Coordinates])
fillAll image = runST $ do
  filledRefs <- V.mapM (V.thaw . (Nothing <$)) image
  let params = FillerParams { paramSourceImage = image
                            , paramFilledRefs  = filledRefs
                            }
  positionTable <- fillAllST `evalStateT` 0 `runReaderT` params
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
fillAllST = fmap IM.fromList $ L.toList $ do
  sourceImage <- lift $ asksView paramSourceImageL
  (y, sourceRow) <- L.fromFoldable $ V.indexed sourceImage
  (x, targetColor) <- L.fromFoldable $ V.indexed sourceRow

  filledRefs <- lift $ asksView paramFilledRefsL
  filledColorMaybe <- lift $ filledRefs V.! y `VM.read` x
  guard $ isNothing filledColorMaybe

  blockIndex <- lift get
  filledPositions <- lift $ fill targetColor blockIndex (x, y)
  lift $ modify (+1)

  pure (blockIndex, filledPositions)

fill ∷ ( Eq a
        , PrimMonad m
        , MonadReader (FillerParams a b (PrimState m)) m
        )
     ⇒ a  -- target color
     → b  -- filling color
     → Coordinates  -- seed
     → m [Coordinates]  -- filled positions
fill targetColor fillingColor seed = execStateT (fill' seed) [] where
  fill' (x, y) = void $ runMaybeT $ do
    sourceImage <- asksView paramSourceImageL
    sourceColor <- hoistMaybe (sourceImage V.!? y >>= (V.!? x))
    guard $ sourceColor == targetColor

    filledRefs <- asksView paramFilledRefsL
    let filledRow = filledRefs V.! y
    Nothing <- VM.read filledRow x

    modify ((x, y) :)
    VM.write filledRow x (Just fillingColor)
    let neighbors = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
    lift $ mapM_ fill' neighbors
