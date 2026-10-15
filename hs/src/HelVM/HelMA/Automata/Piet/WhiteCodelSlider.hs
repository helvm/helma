module HelVM.HelMA.Automata.Piet.WhiteCodelSlider
  ( slideOnWhiteBlock
  ) where

import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.Types.Codel
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.Cursor
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.PointedCodel

import           Control.Monad.Except                             ( MonadError (throwError), liftEither )

import qualified Data.Set                                         as S
import qualified Data.Vector                                      as V

-- Constraint Type Aliases
type MonadNextBlockError m = MonadError (Maybe NextBlock) m
type MonadSlider m = (MonadState (Set Cursor) m, MonadNextBlockError m)

slideOnWhiteBlock ∷ Grid Codel → Cursor → Maybe NextBlock
slideOnWhiteBlock grid cur = either id (error "unreachable") . runIdentity . runExceptT . (`evalStateT` S.empty) $ slideOnWhiteBlockLoop grid cur

slideOnWhiteBlockLoop ∷ MonadSlider m ⇒ Grid Codel → Cursor → m ()
slideOnWhiteBlockLoop grid = fix step where
  step loop cur = processNext loop =<< liftEither (maybeToRight Nothing $ next grid cur)

processNext ∷ MonadSlider m ⇒ (Cursor → m ()) → PointedCodel → m ()
processNext loop pc = checkNonWhite pc.codel.color pc.cursor.course pc.codel.index *> checkVisited pc.cursor *> loop pc.cursor

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

next ∷ Grid Codel → Cursor → Maybe PointedCodel
next grid cur =
  checkCourse grid cur c0 <|>
  checkCourse grid cur c1 <|>
  checkCourse grid cur c2 <|>
  checkCourse grid cur c3 where
    c0 = cur.course
    c1 = succCourse c0
    c2 = succCourse c1
    c3 = succCourse c2

checkCourse ∷ Grid Codel → Cursor → Course → Maybe PointedCodel
checkCourse grid cur nextCourse@(Course nextDP _) = makePair (Cursor pos nextCourse) =<< getNonBlackCodel grid pos where
  !pos = move nextDP cur.position

getNonBlackCodel ∷ Grid Codel → Coordinates → Maybe Codel
getNonBlackCodel (Grid w h cells) (x, y)
  | x >= 0 && x < w && y >= 0 && y < h = checkColor $ V.unsafeIndex cells (y * w + x)
  | otherwise                           = Nothing
