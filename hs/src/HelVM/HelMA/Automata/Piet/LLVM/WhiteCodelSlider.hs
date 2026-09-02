{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}
module HelVM.HelMA.Automata.Piet.LLVM.WhiteCodelSlider
  ( Codel (..)
  , Image
  , slideOnWhiteBlock
  ) where

import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.Cursor
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           Control.Monad.Except                             ( MonadError (throwError), liftEither )

import qualified Data.Set                                         as S
import qualified Data.Vector                                      as V

-- Local Types
type Image = Matrix Codel

data Codel
  = Codel
      { color :: Color
      , index :: Int
      }
  deriving stock (Eq, Show)

-- Constraint Type Aliases
type MonadNextBlockError m = MonadError (Maybe NextBlock) m
type MonadSlider m = (MonadState (Set Cursor) m, MonadNextBlockError m)

slideOnWhiteBlock ∷ Image → Cursor → Maybe NextBlock
slideOnWhiteBlock image cur = either id (error "unreachable") . runIdentity . runExceptT . (`evalStateT` S.empty) $ slideOnWhiteBlockLoop image cur

slideOnWhiteBlockLoop ∷ MonadSlider m ⇒ Image → Cursor → m ()
slideOnWhiteBlockLoop image = fix step where
  step loop cur = processNext loop =<< liftEither (maybeToRight Nothing $ next image cur)

processNext ∷ MonadSlider m ⇒ (Cursor → m ()) → (Codel, Cursor) → m ()
processNext loop (nextCodel, nextCursor) = checkNonWhite nextCodel.color nextCursor.course nextCodel.index *> checkVisited nextCursor *> loop nextCursor

checkNonWhite ∷ MonadNextBlockError m ⇒ Color → Course → Int → m ()
checkNonWhite White _ _              = pass
checkNonWhite _ nextCourse nextIndex = throwError $ Just $ NextBlock NoOperation (BlockEdge nextIndex nextCourse)

checkVisited ∷ MonadSlider m ⇒ Cursor → m ()
checkVisited nextCursor = checkMember nextCursor =<< get

checkMember ∷ MonadSlider m ⇒ Cursor → Set Cursor → m ()
checkMember cur visited = handleVisited (S.member cur visited) cur

handleVisited ∷ MonadSlider m ⇒ Bool → Cursor → m ()
handleVisited True _    = throwError Nothing
handleVisited False cur = modify (S.insert cur)

next ∷ Image → Cursor → Maybe (Codel, Cursor)
next image cur = viaNonEmpty head (mapMaybe (checkCourse image cur) . take 4 $ iterate succCourse cur.course)

checkCourse ∷ Image → Cursor → Course → Maybe (Codel, Cursor)
checkCourse image cur nextCourse@(Course nextDP _) = makePair (Cursor (move nextDP cur.position) nextCourse) =<< getNonBlackCodel image (move nextDP cur.position)

makePair ∷ Cursor → Codel → Maybe (Codel, Cursor)
makePair nextCursor codelInfo = Just (codelInfo, nextCursor)

getNonBlackCodel ∷ Image → Coordinates → Maybe Codel
getNonBlackCodel image (x, y) = checkColor =<< (image V.!? y >>= (V.!? x))

checkColor ∷ Codel → Maybe Codel
checkColor codel
  | codel.color == Black = Nothing
  | otherwise            = Just codel
