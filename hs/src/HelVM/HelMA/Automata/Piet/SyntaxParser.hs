module HelVM.HelMA.Automata.Piet.SyntaxParser
  ( parse
  , parseFilledGrid
  ) where

import           HelVM.HelMA.Automata.Piet.Filler
import           HelVM.HelMA.Automata.Piet.WhiteCodelSlider

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Codel
import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.Cursor
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe

import qualified Data.IntMap                                      as IM
import qualified Data.Map                                         as M
import           Data.MonoTraversable
import qualified Data.Vector                                      as V

import           Relude.Extra

type BlockTable = IntMap BlockCoordinates

parse ∷ MonadSafe m ⇒ Grid Color → m (Maybe SyntaxGraph)
parse grid = parseFilledGridWithSplit (fillAll grid) grid

parseFilledGridWithSplit ∷ MonadSafe m ⇒ (Grid Int, BlockTable) → Grid Color → m (Maybe SyntaxGraph)
parseFilledGridWithSplit (indices, positionTable) grid = parseFilledGrid (zipGridCodel grid indices, positionTable)

parseFilledGrid ∷ MonadSafe m ⇒ (Grid Codel, BlockTable) → m (Maybe SyntaxGraph)
parseFilledGrid (grid, blockTable) = parseFrom grid blockTable =<< searchInitialBlock grid

parseFrom ∷ MonadSafe m ⇒ Grid Codel → BlockTable → Maybe BlockEdge → m (Maybe SyntaxGraph)
parseFrom _ _ Nothing                 = pure Nothing
parseFrom grid blockTable (Just edge) = Just . SyntaxGraph edge <$> execStateT (parseState grid blockTable (view blockIndexL edge)) IM.empty

parseState ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Grid Codel → BlockTable → Int → m ()
parseState grid blockTable blockIndex = processBlockState grid blockTable blockIndex =<< justOrThrow ("MissingCodelIndexError: " <> show blockIndex) (blockTable IM.!? blockIndex)

processBlockState ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Grid Codel → BlockTable → Int → BlockCoordinates → m ()
processBlockState grid blockTable blockIndex = processNextBlockList grid blockTable blockIndex . buildNextBlockList grid

processNextBlockList ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Grid Codel → BlockTable → Int → [(Course, Maybe NextBlock)] → m ()
processNextBlockList grid blockTable blockIndex nextBlockList = processUnvisited grid blockTable nextBlockList =<< insertBlock blockIndex nextBlockList

insertBlock ∷ MonadState (IntMap Block) m ⇒ Int → [(Course, Maybe NextBlock)] → m ()
insertBlock blockIndex nextBlockList = modify (IM.insert blockIndex (Block $ M.fromList nextBlockList))

processUnvisited ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Grid Codel → BlockTable → [(Course, Maybe NextBlock)] → () → m ()
processUnvisited grid blockTable nextBlockList () = traverse_ (parseState grid blockTable) . filterUnvisited nextBlockList =<< get

filterUnvisited ∷ [(Course, Maybe NextBlock)] → IntMap Block → [Int]
filterUnvisited nextBlockList visitedMap =
  filter (`IM.notMember` visitedMap) (mapMaybe (nextBlockToIndex . snd) nextBlockList)

buildNextBlockList ∷ Grid Codel → BlockCoordinates → [(Course, Maybe NextBlock)]
buildNextBlockList grid blockCoords =
  mapMaybe (findCourseNextBlock grid cornerMap curColor blockSize) cursors where
    blockSize = olength blockCoords
    curColor  = getCurColor grid blockCoords
    cursors   = minMaxCoords blockCoords
    cornerMap = M.fromList [ (c.course, c.position) | c <- cursors ]

findCourseNextBlock ∷ Grid Codel → Map Course Coordinates → Maybe ChromaticColor → Int → Cursor → Maybe (Course, Maybe NextBlock)
findCourseNextBlock grid cornerMap curColor blockSize cur = (cur.course,) <$> searchNextBlock grid cornerMap curColor cur.course blockSize

searchInitialBlock ∷ MonadSafe m ⇒ Grid Codel → m (Maybe BlockEdge)
searchInitialBlock grid = processInitial grid =<< justOrThrow "EmptyBlockTableError" (getCodelAt grid (0, 0))

processInitial ∷ MonadSafe m ⇒ Grid Codel → Codel → m (Maybe BlockEdge)
processInitial _ (Codel (Chromatic _) blockIdx) = pure $ Just $ BlockEdge blockIdx initialCourse
processInitial grid (Codel White _)             = pure $ view targetL <$> slideOnWhiteBlock grid initialCursor
processInitial _ (Codel Black _)                = liftError "IllegalInitialColorError"

searchNextBlock ∷ Grid Codel → Map Course Coordinates → Maybe ChromaticColor → Course → Int → Maybe (Maybe NextBlock)
searchNextBlock grid cornerMap curColor startCourse blockSize = tryCourseAttempts grid curColor cornerMap blockSize 0 startCourse

tryCourseAttempts ∷ Grid Codel → Maybe ChromaticColor → Map Course Coordinates → Int → Int → Course → Maybe (Maybe NextBlock)
tryCourseAttempts _    Nothing         _         _         _ _   = Nothing
tryCourseAttempts _    _               _         _         8 _   = Nothing
tryCourseAttempts grid (Just curColor) cornerMap blockSize k crs =
  checkTargetCodel grid curColor crs blockSize (nextAttempt (k + 1) (bounceCourse k crs)) (fetchTargetCodel grid cornerMap crs) where
    nextAttempt = tryCourseAttempts grid (Just curColor) cornerMap blockSize

fetchTargetCodel ∷ Grid Codel → Map Course Coordinates → Course → Maybe (Coordinates, Codel)
fetchTargetCodel grid cornerMap crs = makeTarget =<< M.lookup crs cornerMap where
  makeTarget p = traverseToSnd (fetchNextCodel grid) targetPos where targetPos = move (crs.directionPointer) p

checkTargetCodel ∷ Grid Codel → ChromaticColor → Course → Int → Maybe (Maybe NextBlock) → Maybe (Coordinates, Codel) → Maybe (Maybe NextBlock)
checkTargetCodel _    _        _   _         fallback Nothing                          = fallback
checkTargetCodel _    _        _   _         fallback (Just (_, Codel Black _))        = fallback
checkTargetCodel grid _        crs _         _        (Just (pos, Codel White _))      = Just $ slideOnWhiteBlock grid (Cursor pos crs)
checkTargetCodel _    curColor crs blockSize _        (Just (_, Codel (Chromatic nextColor) nextIdx)) =
  Just $ Just $ NextBlock (commandFromTransition curColor nextColor blockSize) (BlockEdge nextIdx crs)

bounceCourse ∷ Int → Course → Course
bounceCourse k
  | even k    = toggleCodelChooser 1
  | otherwise = rotateDirectionPointer 1

getCurColor ∷ Grid Codel → BlockCoordinates → Maybe ChromaticColor
getCurColor _    []           = Nothing
getCurColor grid ((x, y) : _) = extractChromatic =<< fetchNextCodel grid (x, y)

extractChromatic ∷ Codel → Maybe ChromaticColor
extractChromatic (Codel (Chromatic c) _) = Just c
extractChromatic _                       = Nothing

fetchNextCodel ∷ Grid Codel → Coordinates → Maybe Codel
fetchNextCodel = getCodelAt

getCodelAt ∷ Grid a → Coordinates → Maybe a
getCodelAt (Grid w h cells) (x, y)
  | x >= 0 && x < w && y >= 0 && y < h = cells V.!? (y * w + x)
  | otherwise                           = Nothing

zipGridCodel ∷ Grid Color → Grid Int → Grid Codel
zipGridCodel (Grid w1 h1 v1) (Grid _ _ v2) = Grid w1 h1 (V.zipWith Codel v1 v2)

nextBlockToIndex ∷ Maybe NextBlock → Maybe Int
nextBlockToIndex nb = view (targetL . blockIndexL) <$> nb

minMaxCoords ∷ BlockCoordinates → [Cursor]
minMaxCoords []       = []
minMaxCoords (p : ps) = cursorsFromCorners $ foldl' updateCorners (initCorners p) ps

cursorsFromCorners ∷ Corners → [Cursor]
cursorsFromCorners c =
  [ Cursor c.cR_L (Course DPRight CCLeft)
  , Cursor c.cR_R (Course DPRight CCRight)
  , Cursor c.cD_L (Course DPDown  CCLeft)
  , Cursor c.cD_R (Course DPDown  CCRight)
  , Cursor c.cL_L (Course DPLeft  CCLeft)
  , Cursor c.cL_R (Course DPLeft  CCRight)
  , Cursor c.cU_L (Course DPUp    CCLeft)
  , Cursor c.cU_R (Course DPUp    CCRight)
  ]

updateCorners ∷ Corners → Coordinates → Corners
updateCorners c p = Corners
  (pickRL c.cR_L p)
  (pickRR c.cR_R p)
  (pickDL c.cD_L p)
  (pickDR c.cD_R p)
  (pickLL c.cL_L p)
  (pickLR c.cL_R p)
  (pickUL c.cU_L p)
  (pickUR c.cU_R p)

initCorners ∷ Coordinates → Corners
initCorners p = Corners p p p p p p p p

pickRL ∷ Coordinates → Coordinates → Coordinates
pickRL old@(ox, oy) new@(nx, ny)
  | nx > ox || (nx == ox && ny < oy) = new
  | otherwise                        = old

pickRR ∷ Coordinates → Coordinates → Coordinates
pickRR old@(ox, oy) new@(nx, ny)
  | nx > ox || (nx == ox && ny > oy) = new
  | otherwise                        = old

pickDL ∷ Coordinates → Coordinates → Coordinates
pickDL old@(ox, oy) new@(nx, ny)
  | ny > oy || (ny == oy && nx > ox) = new
  | otherwise                        = old

pickDR ∷ Coordinates → Coordinates → Coordinates
pickDR old@(ox, oy) new@(nx, ny)
  | ny > oy || (ny == oy && nx < ox) = new
  | otherwise                        = old

pickLL ∷ Coordinates → Coordinates → Coordinates
pickLL old@(ox, oy) new@(nx, ny)
  | nx < ox || (nx == ox && ny > oy) = new
  | otherwise                        = old

pickLR ∷ Coordinates → Coordinates → Coordinates
pickLR old@(ox, oy) new@(nx, ny)
  | nx < ox || (nx == ox && ny < oy) = new
  | otherwise                        = old

pickUL ∷ Coordinates → Coordinates → Coordinates
pickUL old@(ox, oy) new@(nx, ny)
  | ny < oy || (ny == oy && nx < ox) = new
  | otherwise                        = old

pickUR ∷ Coordinates → Coordinates → Coordinates
pickUR old@(ox, oy) new@(nx, ny)
  | ny < oy || (ny == oy && nx > ox) = new
  | otherwise                        = old

data Corners
  = Corners
      { cR_L :: !Coordinates
      , cR_R :: !Coordinates
      , cD_L :: !Coordinates
      , cD_R :: !Coordinates
      , cL_L :: !Coordinates
      , cL_R :: !Coordinates
      , cU_L :: !Coordinates
      , cU_R :: !Coordinates
      }

justOrThrow ∷ MonadSafe m ⇒ Message → Maybe a → m a
justOrThrow e = maybe (liftError e) pure
