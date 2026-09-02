module HelVM.HelMA.Automata.Piet.LLVM.WhiteCodelSlider
  ( slideOnWhiteBlock
  ) where

import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.Cursor
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer

import           Control.Monad.Except                             ( MonadError (throwError), liftEither )

import qualified Data.Set                                         as S
import           Data.Vector                                      ( Vector )
import qualified Data.Vector                                      as V

-- Constraint Type Aliases
type MonadNextBlockError m = MonadError (Maybe NextBlock) m
type MonadSlider m = (MonadState (Set (Coordinates, Course)) m, MonadNextBlockError m)

slideOnWhiteBlock ∷ Vector (Vector (Color, Int)) → Cursor → Maybe NextBlock
slideOnWhiteBlock image cur = either id (error "unreachable") . runIdentity . runExceptT . (`evalStateT` S.empty) $ slideOnWhiteBlockLoop image cur

slideOnWhiteBlockLoop ∷ MonadSlider m ⇒ Vector (Vector (Color, Int)) → Cursor → m ()
slideOnWhiteBlockLoop image = fix step where
  step loop cur = processNext loop =<< liftEither (maybeToRight Nothing $ next image cur)

processNext ∷ MonadSlider m ⇒ (Cursor → m ()) → ((Color, Int), Cursor) → m ()
processNext loop ((nextCodel, nextIndex), nextCursor) = checkNonWhite nextCodel nextCursor.course nextIndex *> checkVisited nextCursor *> loop nextCursor

checkNonWhite ∷ MonadNextBlockError m ⇒ Color → Course → Int → m ()
checkNonWhite White _ _              = pass
checkNonWhite _ nextCourse nextIndex = throwError $ Just $ NextBlock NoOperation (BlockEdge nextIndex nextCourse)

checkVisited ∷ MonadSlider m ⇒ Cursor → m ()
checkVisited nextCursor = checkMember (nextCursor.position, nextCursor.course) =<< get

checkMember ∷ MonadSlider m ⇒ (Coordinates, Course) → Set (Coordinates, Course) → m ()
checkMember key visited = handleVisited (S.member key visited) key

handleVisited ∷ MonadSlider m ⇒ Bool → (Coordinates, Course) → m ()
handleVisited True _    = throwError Nothing
handleVisited False key = modify (S.insert key)

next ∷ Vector (Vector (Color, Int)) → Cursor → Maybe ((Color, Int), Cursor)
next image cur = viaNonEmpty head (mapMaybe (checkCourse image cur.position) . take 4 $ iterate succCourse cur.course)

checkCourse ∷ Vector (Vector (Color, Int)) → Coordinates → Course → Maybe ((Color, Int), Cursor)
checkCourse image position nextCourse@(Course nextDP _) = makePair nextCourse (move nextDP position) =<< getNonBlackCodel image (move nextDP position)

makePair ∷ Course → Coordinates → (Color, Int) → Maybe ((Color, Int), Cursor)
makePair nextCourse nextPosition codelInfo = Just (codelInfo, Cursor nextPosition nextCourse)

getNonBlackCodel ∷ Vector (Vector (Color, Int)) → Coordinates → Maybe (Color, Int)
getNonBlackCodel image (x, y) = checkColor =<< (image V.!? y >>= (V.!? x))

checkColor ∷ (Color, Int) → Maybe (Color, Int)
checkColor (Black, _) = Nothing
checkColor codelInfo  = Just codelInfo
