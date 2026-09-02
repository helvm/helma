module HelVM.HelMA.Automata.Piet.LLVM.Parser
  ( ParserError (..)
  , parse
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

import           Control.Monad.Except

import qualified Data.Foldable1                                   as F1
import qualified Data.IntMap                                      as IM
import qualified Data.IntSet                                      as IS
import qualified Data.List.NonEmpty                               as NE
import qualified Data.Map                                         as M
import           Data.MonoTraversable
import qualified Data.Vector                                      as V

import           Relude.Extra

type BlockTable = IntMap BlockCoordinates

data ParserError
  = EmptyBlockTableError
  | IllegalInitialColorError
  | MissingCodelIndexError Int
  deriving stock (Eq, Show)

parse ∷ MonadError ParserError m ⇒ Matrix Color → m (Maybe SyntaxGraph)
parse image = parseFilledImageWithSplit (fillAll image) image

parseFilledImageWithSplit ∷ MonadError ParserError m ⇒ (Matrix Int, BlockTable) → Matrix Color → m (Maybe SyntaxGraph)
parseFilledImageWithSplit (indices, positionTable) image = parseFilledImage (V.zipWith (V.zipWith Codel) image indices, positionTable)

parseFilledImage ∷ MonadError ParserError m ⇒ (Image, BlockTable) → m (Maybe SyntaxGraph)
parseFilledImage (image, blockTable) = parseFrom image blockTable =<< searchInitialBlock image

parseFrom ∷ MonadError ParserError m ⇒ Image → BlockTable → Maybe BlockEdge → m (Maybe SyntaxGraph)
parseFrom _ _ Nothing                  = pure Nothing
parseFrom image blockTable (Just edge) = Just . SyntaxGraph edge <$> execStateT (parseState image blockTable (view blockIndexL edge)) IM.empty

parseState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ Image → BlockTable → Int → m ()
parseState image blockTable blockIndex = justOrThrow (MissingCodelIndexError blockIndex) (blockTable IM.!? blockIndex) >>= processBlockState image blockTable blockIndex

processBlockState ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ Image → BlockTable → Int → BlockCoordinates → m ()
processBlockState image blockTable blockIndex blockCoords = processUnvisited image blockTable (buildNextBlockList image blockCoords) =<< insertBlock blockIndex (buildNextBlockList image blockCoords)

insertBlock ∷ MonadState (IntMap Block) m ⇒ Int → [(Course, Maybe NextBlock)] → m ()
insertBlock blockIndex nextBlockList = modify (IM.insert blockIndex (Block $ M.fromList nextBlockList))

processUnvisited ∷ (MonadError ParserError m, MonadState (IntMap Block) m) ⇒ Image → BlockTable → [(Course, Maybe NextBlock)] → () → m ()
processUnvisited image blockTable nextBlockList () = traverse_ (parseState image blockTable) . filterUnvisited nextBlockList =<< get

filterUnvisited ∷ [(Course, Maybe NextBlock)] → IntMap Block → [Int]
filterUnvisited nextBlockList visitedMap = filter (`IS.notMember` IM.keysSet visitedMap) (mapMaybe (nextBlockToIndex . snd) nextBlockList)

buildNextBlockList ∷ Image → BlockCoordinates → [(Course, Maybe NextBlock)]
buildNextBlockList image blockCoords = mapMaybe (findCourseNextBlock image blockCoords (olength blockCoords)) (minMaxCoords blockCoords)

findCourseNextBlock ∷ Image → BlockCoordinates → Int → Cursor → Maybe (Course, Maybe NextBlock)
findCourseNextBlock image _ blockSize cur = (cur.course,) <$> searchNextBlock image cur.position cur.course blockSize

searchInitialBlock ∷ MonadError ParserError m ⇒ Image → m (Maybe BlockEdge)
searchInitialBlock image = processInitial image =<< justOrThrow EmptyBlockTableError ((V.!? 0) =<< image V.!? 0)

processInitial ∷ MonadError ParserError m ⇒ Image → Codel → m (Maybe BlockEdge)
processInitial _ (Codel (Chromatic _) blockIdx) = pure $ Just $ BlockEdge blockIdx initialCourse
processInitial image (Codel White _)            = pure $ view targetL <$> slideOnWhiteBlock image initialCursor
processInitial _ (Codel Black _)                = throwError IllegalInitialColorError

searchNextBlock ∷ Image → Coordinates → Course → Int → Maybe (Maybe NextBlock)
searchNextBlock image p@(x, y) crs blockSize = searchNextBlockWithColor image p crs blockSize =<< (V.!? x) =<< image V.!? y

searchNextBlockWithColor ∷ Image → Coordinates → Course → Int → Codel → Maybe (Maybe NextBlock)
searchNextBlockWithColor image p crs@(Course dp _) blockSize (Codel (Chromatic curColor) _) = fetchNextCodel image (move dp p) >>= searchNextBlockFromMove image (move dp p) crs blockSize curColor
searchNextBlockWithColor _ _ _ _ _                                                          = Nothing

fetchNextCodel ∷ Image → Coordinates → Maybe Codel
fetchNextCodel image (nextX, nextY) = (V.!? nextX) =<< image V.!? nextY

searchNextBlockFromMove ∷ Image → Coordinates → Course → Int → ChromaticColor → Codel → Maybe (Maybe NextBlock)
searchNextBlockFromMove image nextPos crs blockSize curColor codel = processNextCodel image codel curColor (Cursor nextPos crs) blockSize

processNextCodel ∷ Image → Codel → ChromaticColor → Cursor → Int → Maybe (Maybe NextBlock)
processNextCodel _ (Codel (Chromatic nextColor) blockIdx) curColor cur blockSize = Just $ Just $ NextBlock (commandFromTransition curColor nextColor blockSize) (BlockEdge blockIdx cur.course)
processNextCodel image (Codel White _) _ cur _                                   = Just $ slideOnWhiteBlock image cur
processNextCodel _ (Codel Black _) _ _ _                                         = Nothing

nextBlockToIndex ∷ Maybe NextBlock → Maybe Int
nextBlockToIndex nb = view (targetL . blockIndexL) <$> nb

minMaxCoords ∷ BlockCoordinates → [Cursor]
minMaxCoords positions = processPositions (nonEmpty positions)

processPositions ∷ Maybe (NE.NonEmpty Coordinates) → [Cursor]
processPositions (Just nePositions) = [ Cursor (maximumOn f nePositions) crs | (crs, f) <- fs ]
processPositions Nothing            = []

maximumOn ∷ Ord b ⇒ (a → b) → NE.NonEmpty a → a
maximumOn f = F1.maximumBy (comparing f)

justOrThrow ∷ MonadError e m ⇒ e → Maybe a → m a
justOrThrow e = maybe (throwError e) pure
