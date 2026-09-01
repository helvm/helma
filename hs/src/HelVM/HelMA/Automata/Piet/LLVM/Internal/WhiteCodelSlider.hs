{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.Piet.LLVM.Internal.WhiteCodelSlider
  ( slideOnWhiteBlock
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

import           Control.Monad.Except                             ( MonadError (throwError), liftEither )

import qualified Data.Set                                         as S
import           Data.Vector                                      ( Vector )
import qualified Data.Vector                                      as V

-- Constraint Type Aliases
type MonadNextBlockError m = MonadError NextBlock m
type MonadSlider m = (MonadState (Set (Coordinates, Course)) m, MonadNextBlockError m)

slideOnWhiteBlock ∷ Vector (Vector (Color, Int)) → Coordinates → Course → NextBlock
slideOnWhiteBlock image initialPosition initialCourse' = result where
  result = either id (error "unreachable")
         . runIdentity
         . runExceptT
         . (`evalStateT` S.empty)
         $ slideOnWhiteBlockLoop image initialPosition initialCourse'

slideOnWhiteBlockLoop ∷ MonadSlider m ⇒ Vector (Vector (Color, Int)) → Coordinates → Course → m ()
slideOnWhiteBlockLoop image = fix step where
  step loop position course =
    processNext loop =<< liftEither (maybeToRight ExitProgram $ next image position course)

processNext ∷ MonadSlider m ⇒ (Coordinates → Course → m ()) → ((Color, Int), (Coordinates, Course)) → m ()
processNext loop ((nextCodel, nextIndex), nextCodelCourse@(nextPosition, nextCourse)) =
  checkNonWhite nextCodel nextCourse nextIndex
  *> checkVisited nextCodelCourse
  *> loop nextPosition nextCourse

checkNonWhite ∷ MonadNextBlockError m ⇒ Color → Course → Int → m ()
checkNonWhite White _ _              = pass
checkNonWhite _ nextCourse nextIndex = throwError $ NextBlock NoOperation nextCourse nextIndex

checkVisited ∷ MonadSlider m ⇒ (Coordinates, Course) → m ()
checkVisited nextCodelCourse =
  checkMember nextCodelCourse =<< get

checkMember ∷ MonadSlider m ⇒ (Coordinates, Course) → Set (Coordinates, Course) → m ()
checkMember nextCodelCourse visited =
  handleVisited (S.member nextCodelCourse visited) nextCodelCourse

handleVisited ∷ MonadSlider m ⇒ Bool → (Coordinates, Course) → m ()
handleVisited True _                = throwError ExitProgram
handleVisited False nextCodelCourse = modify (S.insert nextCodelCourse)

next ∷ Vector (Vector (Color, Int)) → Coordinates → Course → Maybe ((Color, Int), (Coordinates, Course))
next image position course = viaNonEmpty head (mapMaybe (checkCourse image position) . take 4 $ iterate succCourse course)

checkCourse ∷ Vector (Vector (Color, Int)) → Coordinates → Course → Maybe ((Color, Int), (Coordinates, Course))
checkCourse image position nextCourse@(Course nextDP _) =
  makePair nextCourse nextPosition =<< getNonBlackCodel image nextPosition where
    nextPosition = move nextDP position

makePair ∷ Course → Coordinates → (Color, Int) → Maybe ((Color, Int), (Coordinates, Course))
makePair nextCourse nextPosition codelInfo = Just (codelInfo, (nextPosition, nextCourse))

getNonBlackCodel ∷ Vector (Vector (Color, Int)) → Coordinates → Maybe (Color, Int)
getNonBlackCodel image (x, y) = checkColor =<< (image V.!? y >>= (V.!? x))

checkColor ∷ (Color, Int) → Maybe (Color, Int)
checkColor (Black, _) = Nothing
checkColor codelInfo  = Just codelInfo
