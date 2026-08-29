{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Internal.Filler
  ( fillAll
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.IntMap                                 as IM
import           Data.Vector                                 ( Vector )
import qualified Data.Vector.Generic                         as V
import           Data.Vector.Mutable                         ( STVector )
import qualified Data.Vector.Mutable                         as VM
import qualified ListT                                       as L

data FillerParams a b s
  = FillerParams
      { _paramSourceImage :: Vector (Vector a)
      , _paramFilledRefs  :: Vector (STVector s (Maybe b))
      }
makeLenses ''FillerParams

fillAll ∷ Eq a ⇒ Vector (Vector a) → (Vector (Vector Int), IntMap [Coordinates])
fillAll image = runST $ do
  filledRefs <- V.mapM (V.thaw . (Nothing <$)) image
  let params = FillerParams { _paramSourceImage = image
                            , _paramFilledRefs = filledRefs
                            }
  positionTable <- fillAllST `evalStateT` 0 `runReaderT` params
  filledImageMaybe <- mapM V.freeze filledRefs
  let filledImage = fmap (fromMaybe 0) <$> filledImageMaybe
  pure (filledImage, positionTable)

fillAllST ∷ ( Eq a
             , PrimMonad m
             , MonadReader (FillerParams a Int (PrimState m)) m
             , MonadState Int m
             )
          ⇒ m (IntMap [Coordinates])
fillAllST = fmap IM.fromList $ L.toList $ do
  sourceImage <- lift $ view paramSourceImage
  (y, sourceRow) <- L.fromFoldable $ V.indexed sourceImage
  (x, targetColor) <- L.fromFoldable $ V.indexed sourceRow

  filledRefs <- lift $ view paramFilledRefs
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
    sourceImage <- view paramSourceImage
    sourceColor <- hoistMaybe (sourceImage V.!? y >>= (V.!? x))
    guard $ sourceColor == targetColor

    filledRefs <- view paramFilledRefs
    let filledRow = filledRefs V.! y
    Nothing <- VM.read filledRow x

    modify ((x, y) :)
    VM.write filledRow x (Just fillingColor)
    let neighbors = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
    lift $ mapM_ fill' neighbors
