module HelVM.HelMA.Automata.Piet.SyntaxParser
  ( parse
  , parseFilledGrid
  ) where

import           HelVM.HelMA.Automata.Piet.Filler
import           HelVM.HelMA.Automata.Piet.WhiteCodelSlider

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Codel
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

import qualified Data.Foldable1                                   as F1
import qualified Data.IntMap                                      as IM
import qualified Data.List.NonEmpty                               as NE
import qualified Data.Map                                         as M
import           Data.MonoTraversable
import qualified Data.Vector                                      as V

import           Relude.Extra

type BlockTable = IntMap BlockCoordinates

parse ∷ MonadSafe m ⇒ Matrix Color → m (Maybe SyntaxGraph)
parse image = parseFilledGridWithSplit (fillAll $ gridToMatrix grid) grid  where grid = (matrixToGrid image)

parseFilledGridWithSplit ∷ MonadSafe m ⇒ (Matrix Int, BlockTable) → Grid Color → m (Maybe SyntaxGraph)
parseFilledGridWithSplit (indices, positionTable) grid = parseFilledGrid (zipGridCodel grid (matrixToGrid indices), positionTable)

parseFilledGrid ∷ MonadSafe m ⇒ (Grid Codel, BlockTable) → m (Maybe SyntaxGraph)
parseFilledGrid (grid, blockTable) = parseFrom grid blockTable =<< searchInitialBlock grid

parseFrom ∷ MonadSafe m ⇒ Grid Codel → BlockTable → Maybe BlockEdge → m (Maybe SyntaxGraph)
parseFrom _ _ Nothing                 = pure Nothing
parseFrom grid blockTable (Just edge) = Just . SyntaxGraph edge <$> execStateT (parseState grid blockTable (view blockIndexL edge)) IM.empty

parseState ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Grid Codel → BlockTable → Int → m ()
parseState grid blockTable blockIndex = justOrThrow ("MissingCodelIndexError: " <> show blockIndex) (blockTable IM.!? blockIndex) >>= processBlockState grid blockTable blockIndex

processBlockState ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Grid Codel → BlockTable → Int → BlockCoordinates → m ()
processBlockState grid blockTable blockIndex blockCoords = processUnvisited grid blockTable (buildNextBlockList grid blockCoords) =<< insertBlock blockIndex (buildNextBlockList grid blockCoords)

insertBlock ∷ MonadState (IntMap Block) m ⇒ Int → [(Course, Maybe NextBlock)] → m ()
insertBlock blockIndex nextBlockList = modify (IM.insert blockIndex (Block $ M.fromList nextBlockList))

processUnvisited ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Grid Codel → BlockTable → [(Course, Maybe NextBlock)] → () → m ()
processUnvisited grid blockTable nextBlockList () = traverse_ (parseState grid blockTable) . filterUnvisited nextBlockList =<< get

filterUnvisited ∷ [(Course, Maybe NextBlock)] → IntMap Block → [Int]
filterUnvisited nextBlockList visitedMap =
  filter (`IM.notMember` visitedMap) (mapMaybe (nextBlockToIndex . snd) nextBlockList)

buildNextBlockList ∷ Grid Codel → BlockCoordinates → [(Course, Maybe NextBlock)]
buildNextBlockList grid blockCoords = mapMaybe (findCourseNextBlock grid blockCoords (olength blockCoords)) (minMaxCoords blockCoords)

findCourseNextBlock ∷ Grid Codel → BlockCoordinates → Int → Cursor → Maybe (Course, Maybe NextBlock)
findCourseNextBlock grid blockCoords blockSize cur = (cur.course,) <$> searchNextBlock grid blockCoords cur.course blockSize

searchInitialBlock ∷ MonadSafe m ⇒ Grid Codel → m (Maybe BlockEdge)
searchInitialBlock grid = processInitial grid =<< justOrThrow "EmptyBlockTableError" (getCodelAt grid (0, 0))

processInitial ∷ MonadSafe m ⇒ Grid Codel → Codel → m (Maybe BlockEdge)
processInitial _ (Codel (Chromatic _) blockIdx) = pure $ Just $ BlockEdge blockIdx initialCourse
processInitial grid (Codel White _)             = pure $ view targetL <$> slideOnWhiteBlock grid initialCursor
processInitial _ (Codel Black _)                = liftError "IllegalInitialColorError"

searchNextBlock ∷ Grid Codel → BlockCoordinates → Course → Int → Maybe (Maybe NextBlock)
searchNextBlock grid blockCoords startCourse blockSize = tryCourseAttempts grid curColor cornerMap blockSize 0 startCourse where
  curColor  = getCurColor grid blockCoords
  cornerMap = M.fromList [ (c.course, c.position) | c <- minMaxCoords blockCoords ]

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
minMaxCoords positions = processPositions (nonEmpty positions)

processPositions ∷ Maybe (NE.NonEmpty Coordinates) → [Cursor]
processPositions (Just nePositions) = [ Cursor (maximumOn f nePositions) crs | (crs, f) <- fs ]
processPositions Nothing            = []

maximumOn ∷ Ord b ⇒ (a → b) → NE.NonEmpty a → a
maximumOn f = F1.maximumBy (comparing f)

justOrThrow ∷ MonadSafe m ⇒ Message → Maybe a → m a
justOrThrow e = maybe (liftError e) pure
