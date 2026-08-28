{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Internal.WhiteCodelSlider
  ( slideOnWhiteBlock
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Cyclic
import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Position
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

import           Control.Monad.Except                             ( MonadError (throwError) )
import qualified Data.Set                                         as S
import           Data.Vector                                      ( Vector )
import qualified Data.Vector                                      as V

slideOnWhiteBlock ∷ Vector (Vector (Codel, Int)) → (Int, Int) → DPCC → NextBlock
slideOnWhiteBlock image initialPosition initialDPCC = result where
  result = (`evalState` S.empty) $ fmap (either id $ error "unreachable") . runExceptT $ slideOnWhiteBlockState initialPosition initialDPCC

  slideOnWhiteBlockState ∷ ( MonadState (Set ((Int, Int), DPCC)) m
                            , MonadError NextBlock m
                            )
                         ⇒ (Int, Int) → DPCC → m ()
  slideOnWhiteBlockState position dpcc = do
    ((nextCodel, nextIndex), nextCodelDPCC@(nextPosition, nextDPCC)) <- maybe (throwError ExitProgram) pure $ next position dpcc
    when (nextCodel /= WhiteCodel) $ throwError $ NextBlock NoOperation nextDPCC nextIndex

    visited <- get
    when (S.member nextCodelDPCC visited) $ throwError ExitProgram
    modify $ S.insert nextCodelDPCC

    slideOnWhiteBlockState nextPosition nextDPCC

  next ∷ (Int, Int) → DPCC → Maybe ((Codel, Int), ((Int, Int), DPCC))
  next position dpcc = listToMaybe $ do
    nextDPCC@(DPCC nextDP _) <- take 4 $ iterate succDPCC dpcc
    let nextPosition = move nextDP position
    codelInfo <- maybeToList $ getNonBlackCodel nextPosition
    pure (codelInfo, (nextPosition, nextDPCC))

  getNonBlackCodel ∷ (Int, Int) → Maybe (Codel, Int)
  getNonBlackCodel (x, y) = do
    (codel, index) <- image V.!? y >>= (V.!? x)
    guard $ codel /= BlackCodel
    pure (codel, index)

succDPCC ∷ DPCC → DPCC
succDPCC (DPCC dp cc) = DPCC (cyclicSucc dp) (cyclicSucc cc)
