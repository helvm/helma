{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.WhiteCodelSlider
  ( slideOnWhiteBlock
  ) where

import           Control.Monad.Except                                  ( MonadError (throwError) )
import qualified Data.Set                                              as S
import           Data.Vector                                           ( Vector )
import qualified Data.Vector                                           as V
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Cyclic
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Position
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax

slideOnWhiteBlock ∷ Vector (Vector (Codel, Int)) → (Int, Int) → DPCC → NextBlock
slideOnWhiteBlock image initialPosition initialDPCC = result where
  result = (`evalState` S.empty) $ fmap (either id $ error "unreachable") . runExceptT $ slideOnWhiteBlockState initialPosition initialDPCC

  slideOnWhiteBlockState ∷ ( MonadState (Set ((Int, Int), DPCC)) m
                            , MonadError NextBlock m
                            )
                         ⇒ (Int, Int) → DPCC → m ()
  slideOnWhiteBlockState position dpcc = do
    (nextCodel, nextIndex, nextPosition, nextDPCC) <- maybe (throwError ExitProgram) pure $ next position dpcc
    when (nextCodel /= WhiteCodel) $ throwError $ NextBlock NoOperation nextDPCC nextIndex

    visited <- get
    let nextCodelDPCC = (nextPosition, nextDPCC)
    when (S.member nextCodelDPCC visited) $ throwError ExitProgram
    modify $ S.insert nextCodelDPCC

    slideOnWhiteBlockState nextPosition nextDPCC

  next ∷ (Int, Int) → DPCC → Maybe (Codel, Int, (Int, Int), DPCC)
  next position dpcc = listToMaybe $ do
    nextDPCC@(DPCC nextDP _) <- take 4 $ iterate succDPCC dpcc
    let nextPosition = move nextDP position
    (nextCodel, nextIndex) <- maybeToList $ getNonBlackCodel nextPosition
    pure (nextCodel, nextIndex, nextPosition, nextDPCC)

  getNonBlackCodel ∷ (Int, Int) → Maybe (Codel, Int)
  getNonBlackCodel (x, y) = do
    (codel, index) <- image V.!? y >>= (V.!? x)
    guard $ codel /= BlackCodel
    pure (codel, index)

succDPCC ∷ DPCC → DPCC
succDPCC (DPCC dp cc) = DPCC (cyclicSucc dp) (cyclicSucc cc)
