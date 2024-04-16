module HelVM.HelMA.Automata.Piet.Automaton where

--import           HelVM.HelMA.Automata.Piet.PietRequest

import           HelVM.HelMA.Automata.Piet.Color
import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.MixedColor

import           HelVM.HelMA.Automata.Piet.Common.EnumExtra

import           HelVM.HelMA.Automata.Piet.MovePointer

---- | Interpret a Piet 'Program'
--interpret' :: Maybe (Lightness, HueColour, Int)  -- ^ Previous' block colour and size if it was a hue-block
--  -> Program        -- ^ Program
--  -> PietMonad ()
interpret' previous program = do
  (x, y)  <- getPosition
  case imgPixel x y (image program) of
    MixedColor l c  -> do
      maybe (return ())
        (\(oldL, oldC, oldS) -> colours2Command oldL oldC l c oldS)
        previous

      dp    <- getDP
      cc    <- getCC
      let key    = imgPixel x y (mask program)
      let label  = findWithDefault EmptyInfo key (info program)

      case nonBlackSucc program label dp cc of
        Just (x', y', dp', cc')  -> do
          setPosition x' y'
          setDP dp'
          setCC cc'
          interpret' (Just (l, c, labelSize label)) program
        Nothing      -> do
          terminate
    White  -> interpretWhite program
    Black  -> do
      logMessage Fatal "Entered black block, terminate"
      terminate

--interpretWhite :: Program -> PietMonad ()
interpretWhite program = do
  (x, y)    <- getPosition
  let key    = imgPixel x y (mask program)
  let codels  = labelSize $ findWithDefault EmptyInfo key (info program)

  -- The Maximum steps without loop:
  --   every DP/CC combination * size of white block
  -- Is there a better way without actually tracking coordinate/DP/CC tuples?

  when (White == imgPixel x y (image program)) $ interpretWhite' (8 * codels) program


--interpretWhite' :: Int -> Program -> PietMonad ()
interpretWhite' limit program is
  | limit <= 0  = terminate
  | otherwise  = do
    xy <- getPosition
    case imgPixel xy (image program) of
      White -> interpretWhite'' limit program is
      _     -> interpret' Nothing program -- found a way out


interpretWhite'' limit program is = interpretWhite' (limit - 1) program (nextInterpreterStatus is)

--runPietMonad :: (PietType -> IO Int)
--  -> (PietType -> Int -> IO ())
--  -> (LogLevel -> String -> IO ())
--  -> PietMonad a
--  -> IO (Either String a)
--runPietMonad readCallback printCallback logCallback program = do
--  requestChannel  <- newChan
--  responseChannel  <- newChan
--  lock    <- newEmptyMVar
--
--  forkIO $ do
--    let PietMonad piet = do
--      x <- program
--      terminate
--
--      pure x
--
--    x <- piet emptyInterpreterStatus
--      requestChannel
--      responseChannel
--
--    putMVar lock x
--
--  let serviceRoutine = do
--    request <- readChan requestChannel
--    case request of
--      Read pType  -> do
--        n <- readCallback pType
--        writeChan responseChannel n
--        serviceRoutine
--      Print pType n  -> do
--        printCallback pType n
--        serviceRoutine
--      Log level msg  -> do
--        logCallback level msg
--        serviceRoutine
--      Terminate  -> return ()
--
--  serviceRoutine
--
--  (liftM fst) `liftM` takeMVar lock



--aaa (Read pType)  = do
--        n <- readCallback pType
--        writeChan responseChannel n
--        serviceRoutine
--aaa (Print pType n)  = do
--        printCallback pType n
--        serviceRoutine
--aaa (Log level msg)  = do
--        logCallback level msg
--        serviceRoutine
--aaa  Terminate = pure ()
