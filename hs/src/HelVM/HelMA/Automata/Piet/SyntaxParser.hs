module HelVM.HelMA.Automata.Piet.SyntaxParser
  ( parse
  , parseFilledImage
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
import           HelVM.HelMA.Automata.Piet.Types.Matrix
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
parse image = parseFilledImageWithSplit (fillAll image) image

parseFilledImageWithSplit ∷ MonadSafe m ⇒ (Matrix Int, BlockTable) → Matrix Color → m (Maybe SyntaxGraph)
parseFilledImageWithSplit (indices, positionTable) image = parseFilledImage (V.zipWith (V.zipWith Codel) image indices, positionTable)

parseFilledImage ∷ MonadSafe m ⇒ (Matrix Codel, BlockTable) → m (Maybe SyntaxGraph)
parseFilledImage (image, blockTable) = parseFrom image blockTable =<< searchInitialBlock image

parseFrom ∷ MonadSafe m ⇒ Matrix Codel → BlockTable → Maybe BlockEdge → m (Maybe SyntaxGraph)
parseFrom _ _ Nothing                  = pure Nothing
parseFrom image blockTable (Just edge) = Just . SyntaxGraph edge <$> execStateT (parseState image blockTable (view blockIndexL edge)) IM.empty

parseState ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Matrix Codel → BlockTable → Int → m ()
parseState image blockTable blockIndex = justOrThrow ("MissingCodelIndexError: " <> show blockIndex) (blockTable IM.!? blockIndex) >>= processBlockState image blockTable blockIndex

processBlockState ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Matrix Codel → BlockTable → Int → BlockCoordinates → m ()
processBlockState image blockTable blockIndex blockCoords = processUnvisited image blockTable (buildNextBlockList image blockCoords) =<< insertBlock blockIndex (buildNextBlockList image blockCoords)

insertBlock ∷ MonadState (IntMap Block) m ⇒ Int → [(Course, Maybe NextBlock)] → m ()
insertBlock blockIndex nextBlockList = modify (IM.insert blockIndex (Block $ M.fromList nextBlockList))

processUnvisited ∷ (MonadSafe m, MonadState (IntMap Block) m) ⇒ Matrix Codel → BlockTable → [(Course, Maybe NextBlock)] → () → m ()
processUnvisited image blockTable nextBlockList () = traverse_ (parseState image blockTable) . filterUnvisited nextBlockList =<< get

filterUnvisited ∷ [(Course, Maybe NextBlock)] → IntMap Block → [Int]
filterUnvisited nextBlockList visitedMap =
  filter (`IM.notMember` visitedMap) (mapMaybe (nextBlockToIndex . snd) nextBlockList)

buildNextBlockList ∷ Matrix Codel → BlockCoordinates → [(Course, Maybe NextBlock)]
buildNextBlockList image blockCoords = mapMaybe (findCourseNextBlock image blockCoords (olength blockCoords)) (minMaxCoords blockCoords)

findCourseNextBlock ∷ Matrix Codel → BlockCoordinates → Int → Cursor → Maybe (Course, Maybe NextBlock)
findCourseNextBlock image blockCoords blockSize cur = (cur.course,) <$> searchNextBlock image blockCoords cur.course blockSize

searchInitialBlock ∷ MonadSafe m ⇒ Matrix Codel → m (Maybe BlockEdge)
searchInitialBlock image = processInitial image =<< justOrThrow "EmptyBlockTableError" ((V.!? 0) =<< image V.!? 0)

processInitial ∷ MonadSafe m ⇒ Matrix Codel → Codel → m (Maybe BlockEdge)
processInitial _ (Codel (Chromatic _) blockIdx) = pure $ Just $ BlockEdge blockIdx initialCourse
processInitial image (Codel White _)            = pure $ view targetL <$> slideOnWhiteBlock image initialCursor
processInitial _ (Codel Black _)                = liftError "IllegalInitialColorError"

searchNextBlock ∷ Matrix Codel → BlockCoordinates → Course → Int → Maybe (Maybe NextBlock)
searchNextBlock image blockCoords startCourse blockSize = tryCourseAttempts image curColor cornerMap blockSize 0 startCourse where
  curColor  = getCurColor image blockCoords
  cornerMap = M.fromList [ (c.course, c.position) | c <- minMaxCoords blockCoords ]

tryCourseAttempts ∷ Matrix Codel → Maybe ChromaticColor → Map Course Coordinates → Int → Int → Course → Maybe (Maybe NextBlock)
tryCourseAttempts _     Nothing         _         _         _ _   = Nothing
tryCourseAttempts _     _               _         _         8 _   = Nothing
tryCourseAttempts image (Just curColor) cornerMap blockSize k crs =
  checkTargetCodel image curColor crs blockSize (nextAttempt (k + 1) (bounceCourse k crs)) (fetchTargetCodel image cornerMap crs) where
    nextAttempt = tryCourseAttempts image (Just curColor) cornerMap blockSize

fetchTargetCodel ∷ Matrix Codel → Map Course Coordinates → Course → Maybe (Coordinates, Codel)
fetchTargetCodel image cornerMap crs = makeTarget =<< M.lookup crs cornerMap where
  makeTarget p = traverseToSnd (fetchNextCodel image) targetPos where targetPos = move (crs.directionPointer) p

checkTargetCodel ∷ Matrix Codel → ChromaticColor → Course → Int → Maybe (Maybe NextBlock) → Maybe (Coordinates, Codel) → Maybe (Maybe NextBlock)
checkTargetCodel _     _        _   _         fallback Nothing                          = fallback
checkTargetCodel _     _        _   _         fallback (Just (_, Codel Black _))        = fallback
checkTargetCodel image _        crs _         _        (Just (pos, Codel White _))      = Just $ slideOnWhiteBlock image (Cursor pos crs)
checkTargetCodel _     curColor crs blockSize _        (Just (_, Codel (Chromatic nextColor) nextIdx)) =
  Just $ Just $ NextBlock (commandFromTransition curColor nextColor blockSize) (BlockEdge nextIdx crs)

bounceCourse ∷ Int → Course → Course
bounceCourse k
  | even k    = toggleCodelChooser 1
  | otherwise = rotateDirectionPointer 1

getCurColor ∷ Matrix Codel → BlockCoordinates → Maybe ChromaticColor
getCurColor _     []           = Nothing
getCurColor image ((x, y) : _) = extractChromatic =<< fetchNextCodel image (x, y)

extractChromatic ∷ Codel → Maybe ChromaticColor
extractChromatic (Codel (Chromatic c) _) = Just c
extractChromatic _                       = Nothing

fetchNextCodel ∷ Matrix Codel → Coordinates → Maybe Codel
fetchNextCodel image (nextX, nextY) = (V.!? nextX) =<< image V.!? nextY

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
