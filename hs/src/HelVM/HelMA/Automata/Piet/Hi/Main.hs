{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module HelVM.HelMA.Automata.Piet.Hi.Main where

import           HelVM.HelIO.ReadText
import           HelVM.HelMA.Automata.Piet.Hi.Types

import           Codec.Picture
import           Control.Monad.Free
import           Control.Monad.Trans.Except

import qualified Data.List                          as L
import           Data.Maybe
import qualified Data.Set                           as S
import           Data.Vector                        ( (!) )
import qualified Data.Vector                        as V

import           Lens.Micro
import           Lens.Micro.Mtl

import           System.Directory                   ( doesFileExist )
import           System.IO                          ( getChar, putChar )

throw ∷ e → ExceptT e IO a
throw = throwE

liftIOExcept ∷ IO a → ExceptT ProgramError IO a
liftIOExcept = lift

posX ∷ (Int → Identity Int) → Position → Identity Position
posX = _1

posY ∷ (Int → Identity Int) → Position → Identity Position
posY = _2

interpret ∷ Program → Piet ()
interpret (Pure _) = pass
interpret (Free c) = case c of
  Push n r -> stack %= (n:) >> interpret r
  Pop r -> stack %= (\s -> (if null s then id else tailUnsafe) s) >> interpret r
  Add r -> perform r (+)
  Subtract r -> perform r subtract
  Multiply r -> perform r (*)
  Divide r -> perform r (flip div)
  Mod r -> perform r (flip mod)
  Not r -> stack %= (\s -> if null s then s else notInstruction (headUnsafe s) : tailUnsafe s) >> interpret r
  Greater r -> perform r greaterInstruction
  Pointer r -> flip' 4 rotatePointer >> interpret r
  Switch r -> flip' 2 toggleChooser >> interpret r
  Duplicate r -> stack %= (\s -> if null s then s else headUnsafe s : s) >> interpret r
  Roll r -> liftPiet (putTextLn "roll") >> do
    s <- use stack
    case s of
      (times:depth:_) -> when (depth >= 0) $ stack %= roll' depth times
      _               -> pass
    interpret r
  InNum r -> maybe pass pushInt <$> liftPiet readInt' >> interpret r
  InChar r -> pushChar <$> liftPiet getChar >> interpret r
  OutNum r -> printTop print' >> interpret r
  OutChar r -> printTop (putChar . chr) >> interpret r
  Nop r -> interpret r
  where manip op = stack %= (\s -> case s of
                                a:b:cs -> a `op` b : cs
                                a      -> a)

        perform r op = manip op >> interpret r

        greaterInstruction a b = if b > a then 1 else 0

        notInstruction 0 = 1
        notInstruction _ = 0

        flip' n f = do
          s <- use stack
          unless (null s) $
            replicateM_ (headUnsafe s `mod` n) f

        readInt' ∷ IO (Maybe Int)
        readInt' = readTextMaybe <$> getLine

        pushInt ∷ Int → Piet ()
        pushInt = (stack %=) . (:)

        pushChar ∷ Char → Piet ()
        pushChar = pushInt . ord

        printTop f = do
          s <- use stack
          unless (null s) $ do
            liftPiet $ f (headUnsafe s)
            stack %= tailUnsafe

        print' = putText . show

        liftPiet = lift . lift

        roll' depth times st
          | depth < 0 = st
          | otherwise =
            let n = times `mod` depth
                (h, t) = (take depth st, drop depth st)
                m = let l = length st in if depth < l then depth else l
            in take m (drop n (cycle h)) <> t

oppositeDir ∷ DirectionPointer → DirectionPointer
oppositeDir d = case d of
  DLeft  -> DRight
  DRight -> DLeft
  DUp    -> DDown
  DDown  -> DUp

move ∷ DirectionPointer → Position → Position
move d = case d of
  DLeft  -> posX -~ 1
  DRight -> posX +~ 1
  DUp    -> posY -~ 1
  DDown  -> posY +~ 1

onMap ∷ ColourMap → Position → Bool
onMap m (i, j) =
  let w = _mapWidth m
      h = _mapHeight m
  in (0 <= i && i < w) && (0 <= j && j < h)

(&!) ∷ ColourMap → Position → Maybe Colour
m &! c@(i, j) =
  if onMap m c
    then Just $ _matrix m ! j ! i
    else Nothing

pixelToColour ∷ PixelRGB8 → Colour
pixelToColour (PixelRGB8 r g b) = case (r, g, b) of
  (255, 192, 192) -> Light Red
  (255, 0, 0)     -> Normal Red
  (192, 0, 0)     -> Dark Red
  (255, 255, 192) -> Light Yellow
  (255, 255, 0)   -> Normal Yellow
  (192, 192, 0)   -> Dark Yellow
  (192, 255, 192) -> Light Green
  (0, 255, 0)     -> Normal Green
  (0, 192, 0)     -> Dark Green
  (192, 255, 255) -> Light Cyan
  (0, 255, 255)   -> Normal Cyan
  (0, 192, 192)   -> Dark Cyan
  (192, 192, 255) -> Light Blue
  (0, 0, 255)     -> Normal Blue
  (0, 0, 192)     -> Dark Blue
  (255, 192, 255) -> Light Magenta
  (255, 0, 255)   -> Normal Magenta
  (192, 0, 192)   -> Dark Magenta
  (0, 0, 0)       -> Black
  _               -> White

imageToColourMap ∷ Image PixelRGB8 → CodelSize → ColourMap
imageToColourMap img cs = ColourMap matrixData w' (V.length matrixData)
  where matrixData = to2D . V.fromList $ map (pixelToColour . pixAt) coords
        coords = [(cx, cy) | cy <- [0..pred h], cx <- [0..pred w], cs |^ cx, cs |^ cy]
        w = imageWidth img
        h = imageHeight img
        w' = w `div` cs
        a |^ b = b `mod` a == 0
        pixAt = uncurry (pixelAt img)

        to2D v = let (hChunk, tChunk) = V.splitAt w' v
                 in if V.length tChunk < w'
                      then V.singleton hChunk
                      else V.cons hChunk (to2D tChunk)

discoverBlock ∷ ColourMap → Position → Block
discoverBlock m c =
  let discover visited c'@(cx, cy) =
        if onMap m c'
          then c' : concatMap (discover (S.insert c' visited)) neighbours
          else []
        where neighbours = filter p [up, down, left, right]
              p = and . sequence [(== colour) . (m &!), onMap m, (`S.notMember` visited)]
              up = (cx, pred cy)
              down = (cx, succ cy)
              left = (pred cx, cy)
              right = (succ cx, cy)
              colour = m &! c'
  in L.nub $ discover S.empty c

hueSteps ∷ Colour → Colour → Maybe Int
hueSteps a b = case (a, b) of
  (Light h1, Light h2)   -> steps h1 h2
  (Normal h1, Normal h2) -> steps h1 h2
  (Dark h1, Dark h2)     -> steps h1 h2
  (Light h1, Normal h2)  -> steps h1 h2
  (Normal h1, Dark h2)   -> steps h1 h2
  (Dark h1, Light h2)    -> steps h1 h2
  (Light h1, Dark h2)    -> steps h1 h2
  (Normal h1, Light h2)  -> steps h1 h2
  (Dark h1, Normal h2)   -> steps h1 h2
  _                      -> Nothing
  where steps h1 h2 = Just $ (fromEnum h2 - fromEnum h1 + 6) `mod` 6

lightnessSteps ∷ Colour → Colour → Maybe Int
lightnessSteps a b = case (a, b) of
  (Light _, Light _)   -> Just 0
  (Normal _, Normal _) -> Just 0
  (Dark _, Dark _)     -> Just 0
  (Light _, Normal _)  -> Just 1
  (Normal _, Dark _)   -> Just 1
  (Dark _, Light _)    -> Just 1
  (Light _, Dark _)    -> Just 2
  (Normal _, Light _)  -> Just 2
  (Dark _, Normal _)   -> Just 2
  (_, _)               -> Nothing

coloursToProgram ∷ Colour → Colour → Int → Program
coloursToProgram c c' n =
  case lightnessSteps c c' of
    Just 0 -> case hueSteps c c' of
      Just 0 -> nop
      Just 1 -> add
      Just 2 -> divide
      Just 3 -> greater
      Just 4 -> duplicate
      Just 5 -> inChar
      _      -> nop
    Just 1 -> case hueSteps c c' of
      Just 0 -> push n
      Just 1 -> subtract'
      Just 2 -> mod'
      Just 3 -> pointer
      Just 4 -> roll
      Just 5 -> outNum
      _      -> nop
    Just 2 -> case hueSteps c c' of
      Just 0 -> pop
      Just 1 -> multiply
      Just 2 -> not'
      Just 3 -> switch
      Just 4 -> inNum
      Just 5 -> outChar
      _      -> nop
    _ -> nop

transition ∷ Piet ()
transition = do
  cc <- use collisionCount
  when (cc == 7) $ liftPiet exitSuccess
  dp <- use directionPointer
  pos <- use currentPosition
  m <- asks colourMap
  let block = discoverBlock m pos
  p <- selectCodel block
  let newPos = move dp p
  colour <- colourAt newPos
  case colour of
    Nothing -> doIfCollided cc block
    Just Black -> doIfCollided cc block
    Just c' -> do
      collisionCount .= 0
      currentPosition .= newPos
      Just c <- colourAt p
      let numCodels = length block
          instr = coloursToProgram c c' numCodels
      interpret instr
  where doIfCollided ∷ Int → Block → Piet ()
        doIfCollided cc block = do
          if even cc
            then toggleChooser
            else rotatePointer
          newPos <- selectCodel block
          currentPosition .= newPos
          collisionCount += 1
          transition

        liftPiet = lift . lift

colourAt ∷ Position → Piet (Maybe Colour)
colourAt pos = (&! pos) <$> asks colourMap

toggleChooser ∷ Piet ()
toggleChooser = do
  cc <- use codelChooser
  codelChooser .= if cc == CLeft
                    then CRight
                    else CLeft

rotatePointer ∷ Piet ()
rotatePointer = do
  dp <- use directionPointer
  directionPointer .= case dp of
    DLeft  -> DUp
    DUp    -> DRight
    DRight -> DDown
    DDown  -> DLeft

selectCodel ∷ Block → Piet Position
selectCodel block = do
  dp <- use directionPointer
  cc <- use codelChooser
  return $ L.maximumBy (furthest dp cc) block
  where furthest dp cc = case cc of
          CLeft -> case dp of
            DLeft  -> flip (comparing fst) <> comparing snd
            DRight -> comparing fst <> flip (comparing snd)
            DUp    -> flip (comparing snd <> comparing fst)
            DDown  -> comparing snd <> comparing fst
          CRight -> case dp of
            DLeft  -> flip (comparing fst <> comparing snd)
            DRight -> comparing fst <> comparing snd
            DUp    -> flip (comparing snd) <> comparing fst
            DDown  -> comparing snd <> flip (comparing fst)

execute ∷ CodelSize → Image PixelRGB8 → IO ()
execute cs img =
  let conf = ProgramConfig {
          codelSize = cs,
          colourMap = imageToColourMap img cs
        }
  in void $ runPiet conf initialState (forever transition)

main ∷ IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> help
    [fp, "-cs", n] -> do
      v <- runExceptT $ something fp (toText n)
      case v of
        Left e          -> print e >> exitFailure
        Right (img, cs) -> execute cs img
    _ -> repl

  where help = putTextLn $ unlines ["", "Help", "----", "..."]

        repl = putTextLn "repl"

        something fp n = do
          let n' = readTextMaybe n
          when (isNothing n') $ throw $ ParseInt $ n <> " as an int."
          let cs = fromJust n'
          fileExists <- liftIOExcept $ doesFileExist fp
          unless fileExists $ throw $ FindFile $ toText fp
          loadedImg <- liftIOExcept $ readImage fp
          case loadedImg of
            Left _ -> throw $ LoadFile $ toText fp
            Right dynamicImg -> case dynamicImg of
              ImageRGB8 parsedImg -> return (parsedImg, cs)
              ImageY8 _           -> liftIOExcept (putTextLn "Y8") >> throw (NotImplemented "Y8 format")
              ImageYF _           -> liftIOExcept (putTextLn "YF") >> throw (NotImplemented "YF format")
              ImageYA8 _          -> liftIOExcept (putTextLn "YA8") >> throw (NotImplemented "YA8 format")
              ImageRGBA8 _        -> liftIOExcept (putTextLn "RGBA8") >> throw (NotImplemented "RGBA8 format")
              ImageRGBF _         -> liftIOExcept (putTextLn "RGBF") >> throw (NotImplemented "RGBF format")
              ImageYCbCr8 _       -> liftIOExcept (putTextLn "YCbCr8") >> throw (NotImplemented "YCbCr8 format")
              _                   -> throw (NotImplemented "Other image formats")

--[ tests ]--

testMap ∷ ColourMap
testMap = ColourMap {
    _matrix =
       V.fromList [V.fromList [Normal Blue,Normal Blue,Normal Blue,Dark Blue,Dark Blue,Dark Blue,
                               Black,Normal Green,Normal Green,Black],
                   V.fromList [Normal Blue,Normal Blue,Normal Blue,Dark Blue,Dark Blue,Normal Green,
                               Normal Green,Normal Green,Normal Green,Normal Green],
                   V.fromList [Normal Blue,Normal Blue,Normal Blue,Dark Blue,Dark Blue,Dark Blue,
                               Light Blue,Dark Magenta,Dark Cyan,Normal Green],
                   V.fromList [Normal Red,Normal Red,Normal Red,Normal Red,Black,Normal Red,
                               Normal Red,Normal Cyan,Normal Red,Normal Green],
                   V.fromList [Normal Red,Normal Red,Normal Red,Normal Red,Normal Green,Dark Cyan,
                               Normal Cyan,Normal Cyan,Normal Cyan,Normal Green],
                   V.fromList [Normal Red,Normal Red,Normal Red,Normal Red,Normal Green,Normal Red,
                               Normal Red,Normal Cyan,Dark Green,Normal Green],
                   V.fromList [Normal Yellow,Normal Yellow,Black,Black,Normal Green,Black,Black,
                               Normal Cyan,Dark Green,Dark Green],
                   V.fromList [Normal Yellow,Normal Yellow,Black,Normal Green,Normal Green,Normal Green,
                               Black,Normal Cyan,Normal Cyan,Light Green],
                   V.fromList [Black,Black,Black,Black,Black,Black,Black,Black,Normal Cyan,Dark Cyan],
                   V.fromList [Normal Yellow,Normal Yellow,Black,Normal Yellow,Normal Yellow,
                               Normal Yellow,Normal Yellow,Black,Normal Cyan,Dark Blue]],
    _mapWidth = 10,
    _mapHeight = 10
  }

runProgram ∷ Program → IO ()
runProgram = void . runPiet (error "No config provided for test run") initialState . interpret

testProgram ∷ Program
testProgram = do
  let helloworld = "Hello world\n"
  mapM_ (push . ord) (reverse helloworld)
  replicateM_ (length helloworld) outChar

headUnsafe ∷ [a] → a
headUnsafe []      = error "headUnsafe: empty list"
headUnsafe (a : _) = a

tailUnsafe ∷ [a] → [a]
tailUnsafe []       = error "tailUnsafe: empty list"
tailUnsafe (_ : xs) = xs
