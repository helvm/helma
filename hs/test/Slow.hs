{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

module Slow
    ( configure
    , timeThese
    , timedHspec
    , timedHspecParallel
    ) where

import           Control.Concurrent.STM.TVar hiding (newTVarIO, readTVarIO)
import           Data.Time.Clock
import           Test.Hspec
import           Test.Hspec.Core.Spec

type SlowResults = [(String, NominalDiffTime)]
type SlowResultTracker = TVar SlowResults

data SlowConfiguration
  = SlowConfiguration
      { duration :: !Int
      , tracker  :: !SlowResultTracker
      }

configure ∷ Int → IO SlowConfiguration
configure x =
  newTVarIO [] >>= \t ->
    pure (SlowConfiguration x t)

stopwatch ∷ MonadIO m ⇒ m a → m (a, NominalDiffTime)
stopwatch x = do
  start <- liftIO getCurrentTime
  !a <- x
  end <- liftIO getCurrentTime
  pure (a, end `diffUTCTime` start)

trackedAction ∷ MonadIO m ⇒ String → m a → ReaderT SlowConfiguration m a
trackedAction s m = do
  conf <- ask
  (result, d) <- lift (stopwatch m)
  if d > (realToFrac . duration $ conf)
    then do
      liftIO $ atomically $ modifyTVar (tracker conf) (++ [(s, d)])
      pure result
    else pure result

type Timer = forall m a. (MonadIO m, Example (m a)) ⇒ String → m a → SpecWith (Arg (m a))

timed ∷ SlowConfiguration → Timer
timed c s a = it s $ runReaderT (trackedAction s a) c

slowReport ∷ (MonadIO m) ⇒ SlowConfiguration → m ()
slowReport s = do
  liftIO $ do
    slows <- readTVarIO (tracker s)
    putStrLn "Slow examples:"
    mapM_ (\(t, v) -> putStrLn $ show v ++ ": " ++ t) slows

timedHspec ∷ SlowConfiguration → (Timer → SpecWith ()) → IO ()
timedHspec t x = hspec $ (afterAll_ . slowReport) t $ x (timed t)

timedHspecParallel ∷ SlowConfiguration → (Timer → SpecWith ()) → IO ()
timedHspecParallel t x = hspec $ (afterAll_ . slowReport) t $ parallel $ x (timed t)

-- | times all tests without having to use a custom `it` function
timeThese ∷ SlowConfiguration → SpecWith a → SpecWith a
timeThese config = afterAll_ (slowReport config) . mapSpecItem_ (modifyAroundAction $ adhocMeasure config)

adhocMeasure ∷ SlowConfiguration → Item a → (a → IO ()) → a → IO ()
adhocMeasure config item theTestF a = runReaderT (trackedAction (makeDescription item) $ theTestF a) config

makeDescription ∷ Item a → String
makeDescription item = defaultDescription (itemLocation item)  <> "\n\t" <>
  itemRequirement item

defaultDescription ∷ Maybe Location → String
defaultDescription stack = fromMaybe ("source location not found: " <> show stack)  $ do
  Location locationFile locationLine locationColumn <- stack
  pure (locationFile ++ "[" ++ show locationLine ++ ":" ++ show locationColumn ++ "]")

modifyAroundAction ∷ (Item a → ActionWith a → ActionWith b) → Item a → Item b
modifyAroundAction action item@Item{itemExample = e} =
  item{ itemExample = \params aroundAction -> e params (aroundAction . action item) }
