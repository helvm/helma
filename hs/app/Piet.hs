{-# LANGUAGE TemplateHaskell, DeriveFunctor, RecordWildCards, GeneralizedNewtypeDeriving #-}
-- An interpreter for the esoteric Piet language, see http://www.dangermouse.net/esoteric/piet.html
-- for the specification.
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad (when, unless, replicateM_, forever, void)
import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Codec.Picture

import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord
import qualified Data.List as L
import qualified Data.Set as S
import Data.Vector ((!))
import qualified Data.Vector as V

import Piet.Types

import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit
import System.Posix.Unistd

--[ utility functions ]--
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _ -> Nothing

throw = left
io = lift

x = _1
y = _2

interpret :: Program -> Piet ()
interpret (Pure _) = return ()
interpret (Free c) = case c of
  Push n r -> {-io (putStrLn ("push " ++ show n)) >>-} stack %= (n:) >> interpret r
  Pop r -> {-io (putStrLn "pop") >>-} stack %= (\s -> (if null s then id else tail) s) >> interpret r
  Add r -> {-io (putStrLn "add") >>-} perform r (+)
  Subtract r -> {-io (putStrLn "subtract") >>-} perform r subtract
  Multiply r -> {-io (putStrLn "mutliply") >>-} perform r (*)
  Divide r -> {-io (putStrLn "divide") >>-} perform r (flip div)
  Mod r -> {-io (putStrLn "mod") >>-} perform r (flip mod)
  Not r -> {-io (putStrLn "not") >>-} stack %= (\s -> if null s then s else not' (head s):tail s) >> interpret r  
  Greater r -> {-io (putStrLn "greater") >>-} perform r greater
  Pointer r -> {-io (putStrLn "pointer") >>-} flip' 4 rotatePointer >> interpret r
  Switch r -> {-io (putStrLn "switch") >>-} flip' 2 toggleChooser >> interpret r
  Duplicate r -> {-io (putStrLn "duplicate") >>-} stack %= (\s -> if null s then s else head s:s) >> interpret r
  Roll r -> io (putStrLn "roll") >> do
    s <- use stack
    case s of
      a@(times:depth:_) -> when (depth >= 0) $ stack %= roll' depth times
      _ -> return ()
    interpret r
  InNum r -> {-io (putStrLn "innum") >>-} maybe (return ()) pushInt <$> io readInt' >> interpret r
  InChar r -> {-io (putStrLn "inchar") >>-} pushChar <$> io getChar >> interpret r
  OutNum r -> {-io (putStrLn "outnum") >>-} printTop print' >> interpret r
  OutChar r -> {-io (putStrLn "outchar") >>-} printTop (putChar . chr) >> interpret r
  Nop r -> {-io (putStrLn "nop") >>-} interpret r
  where manip op = stack %= (\s -> case s of
                                a:b:cs -> a `op` b:cs
                                a -> a)
        
        perform r op = manip op >> interpret r
        
        a `greater` b = if b > a then 1 else 0
        
        not' 0 = 1
        not' _ = 0
        
        flip' n f = do
          s <- use stack
          unless (null s) $
            -- modulo n, because f behaves cyclically with period n
            replicateM_ (head s `mod` n) f
          
        readInt' :: IO (Maybe Int)
        readInt' = readMaybe <$> getLine
        
        pushInt :: Int -> Piet ()
        pushInt = (stack %=) . (:)
        
        pushChar :: Char -> Piet ()
        pushChar = pushInt . ord
        
        printTop f = do
          s <- use stack
          unless (null s) $ do
            io $ f (head s)
            stack %= tail
            
        print' = putStr . show

        io = lift . lift

        roll' depth times stack
          | depth < 0 = stack
          | otherwise =
            let n = times `mod` depth
                (h, t) = (take depth stack, drop depth stack)
                m = let l = length stack in if depth < l then depth else l
            in take m (drop n (cycle h)) ++ t

oppositeDir :: DirectionPointer -> DirectionPointer
oppositeDir d = case d of
  DLeft -> DRight
  DRight -> DLeft
  DUp -> DDown
  DDown -> DUp

move :: DirectionPointer -> Position -> Position
move d = case d of
  DLeft -> x -~ 1
  DRight -> x +~ 1
  DUp -> y -~ 1
  DDown -> y +~ 1

onMap :: ColourMap -> Position -> Bool
onMap m (i, j) =
  let w = _mapWidth m
      h = _mapHeight m
  in (0 <= i && i < w) && (0 <= j && j < h)
     
(&!) :: ColourMap -> Position -> Maybe Colour
m &! c@(i, j) =
  if onMap m c
    then Just $ _matrix m ! j ! i
    else Nothing
  
pixelToColour :: PixelRGB8 -> Colour
pixelToColour (PixelRGB8 r g b) = case (r, g, b) of
  (255, 192, 192) -> Light Red
  (255, 0, 0) -> Normal Red
  (192, 0, 0) -> Dark Red
  (255, 255, 192) -> Light Yellow
  (255, 255, 0) -> Normal Yellow
  (192, 192, 0) -> Dark Yellow
  (192, 255, 192) -> Light Green
  (0, 255, 0) -> Normal Green
  (0, 192, 0) -> Dark Green
  (192, 255, 255) -> Light Cyan
  (0, 255, 255) -> Normal Cyan
  (0, 192, 192) -> Dark Cyan
  (192, 192, 255) -> Light Blue
  (0, 0, 255) -> Normal Blue
  (0, 0, 192) -> Dark Blue
  (255, 192, 255) -> Light Magenta
  (255, 0, 255) -> Normal Magenta
  (192, 0, 192) -> Dark Magenta
  (0, 0, 0) -> Black
  _ -> White
   
imageToColourMap :: Image PixelRGB8 -> CodelSize -> ColourMap
imageToColourMap img cs = ColourMap matrix w' (V.length matrix)
  where matrix = to2D . V.fromList $ map (pixelToColour . pixAt) coords
        coords = [(x, y) | y <- [0..pred h], x <- [0..pred w], cs |^ x, cs |^ y]
        w = imageWidth img
        h = imageHeight img
        w' = w `div` cs
        a |^ b = b `mod` a == 0 -- a divides b
        pixAt = uncurry (pixelAt img)
        
        -- Converts a 1D array to a 2D array in row order.
        to2D v = let (head, tail) = V.splitAt w' v
                 in if V.length tail < w'
                      then V.singleton head
                      else V.cons head (to2D tail)

-- Finds all the codels which are in the same colour block as c.
discoverBlock :: ColourMap -> Position -> Block
discoverBlock m c =
  let discover visited c'@(x, y) =
        if onMap m c'
          then c':concatMap (discover (S.insert c' visited)) neighbours
          else []
        where neighbours = filter p [up, down, left, right]
              p = and . sequence [(== colour) . (m &!), onMap m, (`S.notMember` visited)]
              up = (x, pred y)
              down = (x, succ y)
              left = (pred x, y)
              right = (succ x, y)
              colour = m &! c'
  in L.nub $ discover S.empty c
                                       
-- How many steps do we need to take from a to b? 'hueSteps a b' is the answer.
hueSteps :: Colour -> Colour -> Maybe Int
hueSteps a b = case (a, b) of
  (Light x, Light y) -> steps x y
  (Normal x, Normal y) -> steps x y
  (Dark x, Dark y) -> steps x y
  (Light x, Normal y) -> steps x y
  (Normal x, Dark y) -> steps x y
  (Dark x, Light y) -> steps x y
  (Light x, Dark y) -> steps x y
  (Normal x, Light y) -> steps x y
  (Dark x, Normal y) -> steps x y
  _ -> Nothing
  where steps x y = Just $ (fromEnum y - fromEnum x + 6) `mod` 6

lightnessSteps :: Colour -> Colour -> Maybe Int
lightnessSteps a b = case (a, b) of
  (Light _, Light _) -> Just 0
  (Normal _, Normal _) -> Just 0
  (Dark _, Dark _) -> Just 0
  (Light _, Normal _) -> Just 1
  (Normal _, Dark _) -> Just 1
  (Dark _, Light _) -> Just 1
  (Light _, Dark _) -> Just 2
  (Normal _, Light _) -> Just 2
  (Dark _, Normal _) -> Just 2
  (_, _) -> Nothing

-- Takes two colours, calculates their lightness and hue diffs,
-- and an int which is passed to push, and returns an instruction
-- for the interpreter.
coloursToProgram :: Colour -> Colour -> Int -> Program
coloursToProgram c c' n =
  case lightnessSteps c c' of
    Just 0 -> case hueSteps c c' of
      Just 0 -> nop
      Just 1 -> add
      Just 2 -> divide
      Just 3 -> greater
      Just 4 -> duplicate
      Just 5 -> inChar
      _ -> nop
    Just 1 -> case hueSteps c c' of
      Just 0 -> push n
      Just 1 -> subtract'
      Just 2 -> mod'
      Just 3 -> pointer
      Just 4 -> roll
      Just 5 -> outNum
      _ -> nop
    Just 2 -> case hueSteps c c' of
      Just 0 -> pop
      Just 1 -> multiply
      Just 2 -> not'
      Just 3 -> switch
      Just 4 -> inNum
      Just 5 -> outChar
      _ -> nop
    _ -> nop

-- Transitions from the currentPosition to the new position
-- in the direction of directionPointer and executes the command.
-- Halts the interpreter after 8 consecutive collisions.
-- Transitions again when it collides.
transition :: Piet ()
transition = do
  cc <- use collisionCount
  when (cc == 7) $ io exitSuccess
  dp <- use directionPointer
  pos <- use currentPosition
  m <- asks colourMap
  let block = discoverBlock m pos
  p <- selectCodel block
  let newPos = move dp p
  colour <- colourAt newPos
  case colour of
    Nothing -> doIfCollided cc m p block
    Just Black -> doIfCollided cc m p block
    Just c' -> do
      collisionCount .= 0 -- reset the collision count
      currentPosition .= newPos -- move to the new position
      Just c <- colourAt p -- the current position is a legal position on the map
      let numCodels = length block
          instr = coloursToProgram c c' numCodels
      -- io $ putStrLn $ "moving from " ++ show c ++ " to " ++ show c'
      interpret instr
      -- io $ void $ sleep 1
  where doIfCollided :: Int -> ColourMap -> Position -> Block -> Piet ()
        doIfCollided cc m cp block = do
          if even cc
            then toggleChooser
            else rotatePointer
          newPos <- selectCodel block
          currentPosition .= newPos
          collisionCount += 1
          transition
          
        io = lift . lift

-- Returns the colour at pos on colourMap wrapped in a Just.
-- When pos isn't within the bounds of the map, colourAt returns Nothing.
colourAt :: Position -> Piet (Maybe Colour)
colourAt pos = (&! pos) <$> asks colourMap

toggleChooser :: Piet ()
toggleChooser = do
  cc <- use codelChooser
  codelChooser .= if cc == CLeft
                    then CRight
                    else CLeft

-- Rotates directionPointer clockwise.
rotatePointer :: Piet ()
rotatePointer = do
  dp <- use directionPointer
  directionPointer .= case dp of
    DLeft -> DUp
    DUp -> DRight
    DRight -> DDown
    DDown -> DLeft

-- Returns the coordinates of the codel from which we should transition next.
selectCodel :: Block -> Piet Position
selectCodel block = do
  dp <- use directionPointer
  cc <- use codelChooser
  return $ L.maximumBy (furthest dp cc) block
  where furthest dp cc = case cc of
          CLeft -> case dp of
            DLeft -> flip (comparing fst) <> comparing snd
            DRight -> comparing fst <> flip (comparing snd)
            DUp -> flip (comparing snd <> comparing fst)
            DDown -> comparing snd <> comparing fst
          CRight -> case dp of
            DLeft -> flip (comparing fst <> comparing snd)
            DRight -> comparing fst <> comparing snd
            DUp -> flip (comparing snd) <> comparing fst
            DDown -> comparing snd <> flip (comparing fst)

execute cs img =
  let conf = ProgramConfig {
          codelSize = cs,
          colourMap = imageToColourMap img cs
        }          
  in void $ runPiet conf initialState (forever transition)         
    
main = do
  args <- getArgs
  case args of
    ["--help"] -> help
    [fp, "-cs", n] -> do
      v <- runEitherT $ something fp n
      case v of
        Left e -> print e >> exitFailure
        Right (img, cs) -> execute cs img
    _ -> repl
    
  where help = putStrLn $ unlines ["", "Help", "----", "..."]
        
        repl = putStrLn "repl"
        
        something fp n = do
          let n' = readMaybe n
          when (isNothing n') $ throw $ ParseInt $ n ++ " as an int."
          let cs = fromJust n'
          e <- io $ doesFileExist fp
          unless e $ throw $ FindFile fp
          img <- io $ readImage fp
          case img of
            Left e -> throw $ LoadFile fp
            Right a -> case a of
              ImageRGB8 img -> return (img, cs)
              ImageY8 _ -> io (putStrLn "Y8") >> throw (NotImplemented "Y8 format")
              ImageYF _ -> io (putStrLn "YF") >> throw (NotImplemented "YF format")
              ImageYA8 _ -> io (putStrLn "YA8") >> throw (NotImplemented "YA8 format")
              ImageRGBA8 _ -> io (putStrLn "RGBA8") >> throw (NotImplemented "RGBA8 format")
              ImageRGBF _ -> io (putStrLn "RGBF") >> throw (NotImplemented "RGBF format")
              ImageYCbCr8 _ -> io (putStrLn "YCbCr8") >> throw (NotImplemented "YCbCr8 format")

--[ tests ]--

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

runProgram :: Program -> IO ()
runProgram = void . runPiet undefined initialState . interpret

-- Should print "Hello world" to stdout when interpreted by 'interpret'
testProgram :: Program
testProgram = do
  let helloworld = "Hello world\n"
  mapM_ (push . ord) (reverse helloworld)  
  replicateM_ (length helloworld) outChar
