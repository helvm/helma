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
import           HelVM.HelMA.Automata.Piet.Types.Matrix
import           HelVM.HelMA.Automata.Piet.Types.PointedCodel

import           Control.Monad.Except                             ( MonadError (throwError), liftEither )

import qualified Data.Set                                         as S
import qualified Data.Vector                                      as V

-- Constraint Type Aliases
type MonadNextBlockError m = MonadError (Maybe NextBlock) m
type MonadSlider m = (MonadState (Set Cursor) m, MonadNextBlockError m)

slideOnWhiteBlock ∷ Matrix Codel → Cursor → Maybe NextBlock
slideOnWhiteBlock image cur = either id (error "unreachable") . runIdentity . runExceptT . (`evalStateT` S.empty) $ slideOnWhiteBlockLoop image cur

slideOnWhiteBlockLoop ∷ MonadSlider m ⇒ Matrix Codel → Cursor → m ()
slideOnWhiteBlockLoop image = fix step where
  step loop cur = processNext loop =<< liftEither (maybeToRight Nothing $ next image cur)

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

next ∷ Matrix Codel → Cursor → Maybe PointedCodel
next image cur = viaNonEmpty head (mapMaybe (checkCourse image cur) . take 4 $ iterate succCourse cur.course)

checkCourse ∷ Matrix Codel → Cursor → Course → Maybe PointedCodel
checkCourse image cur nextCourse@(Course nextDP _) = makePair (Cursor (move nextDP cur.position) nextCourse) =<< getNonBlackCodel image (move nextDP cur.position)

getNonBlackCodel ∷ Matrix Codel → Coordinates → Maybe Codel
getNonBlackCodel image (x, y) = checkColor =<< (image V.!? y >>= (V.!? x))
