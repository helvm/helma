{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Internal.WhiteCodelSlider
  ( slideOnWhiteBlock
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Internal.Position
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Cyclic

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Control.Monad.Except                             ( MonadError (throwError) )
import qualified Data.Set                                         as S
import           Data.Vector                                      ( Vector )
import qualified Data.Vector                                      as V

slideOnWhiteBlock ∷ Vector (Vector (Color, Int)) → Coordinates → Course → NextBlock
slideOnWhiteBlock image initialPosition initialCourse = result where
  result = (`evalState` S.empty) $ fmap (either id $ error "unreachable") . runExceptT $ slideOnWhiteBlockState initialPosition initialCourse

  slideOnWhiteBlockState ∷ ( MonadState (Set (Coordinates, Course)) m
                            , MonadError NextBlock m
                            )
                         ⇒ Coordinates → Course → m ()
  slideOnWhiteBlockState position course = do
    ((nextCodel, nextIndex), nextCodelCourse@(nextPosition, nextCourse)) <- maybe (throwError ExitProgram) pure $ next position course
    when (nextCodel /= White) $ throwError $ NextBlock NoOperation nextCourse nextIndex

    visited <- get
    when (S.member nextCodelCourse visited) $ throwError ExitProgram
    modify $ S.insert nextCodelCourse

    slideOnWhiteBlockState nextPosition nextCourse

  next ∷ Coordinates → Course → Maybe ((Color, Int), (Coordinates, Course))
  next position course = listToMaybe $ do
    nextCourse@(Course nextDP _) <- take 4 $ iterate succCourse course
    let nextPosition = move nextDP position
    codelInfo <- maybeToList $ getNonBlackCodel nextPosition
    pure (codelInfo, (nextPosition, nextCourse))

  getNonBlackCodel ∷ Coordinates → Maybe (Color, Int)
  getNonBlackCodel (x, y) = do
    (codel, index) <- image V.!? y >>= (V.!? x)
    guard $ codel /= Black
    pure (codel, index)

succCourse ∷ Course → Course
succCourse (Course dp cc) = Course (cyclicSucc dp) (cyclicSucc cc)
