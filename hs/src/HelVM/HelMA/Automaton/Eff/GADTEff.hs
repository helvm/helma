{-# LANGUAGE GADTs #-}
module HelVM.HelMA.Automaton.Eff.GADTEff
  ( GADTEff
  , GADTEffF (..)
  , interpretGADTEff
  , interpretGADTEffDebug
  ) where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

newtype GADTEff a
  = GADTEff { runGADTEff :: forall m. Monad m => (forall x. GADTEffF x -> m x) -> m a }

instance Functor GADTEff where
  fmap f m = GADTEff $ \k -> fmap f (runGADTEff m k)

instance Applicative GADTEff where
  pure a  = GADTEff $ \_ -> pure a
  f <*> a = GADTEff $ \k -> runGADTEff f k <*> runGADTEff a k

instance Monad GADTEff where
  return = pure
  m >>= f = GADTEff $ \k -> runGADTEff m k >>= \a -> runGADTEff (f a) k

liftF ∷ GADTEffF a → GADTEff a
liftF fa = GADTEff $ \k -> k fa

--------------------------------------------------------------------------------
-- Interpretacja

interpretGADTEffDebug ∷ AppEff m ⇒ GADTEff a → m a
interpretGADTEffDebug eff = runGADTEff eff interpretGADTEffFDebug

interpretGADTEff ∷ MonadEff m ⇒ GADTEff a → m a
interpretGADTEff eff = runGADTEff eff interpretGADTEffF

--------------------------------------------------------------------------------
-- Interpreter dla pojedynczych instrukcji (bez fmap/kontynuacji!)

interpretGADTEffFDebug ∷ AppEff m ⇒ GADTEffF a → m a
interpretGADTEffFDebug GetContentsBS   = logDebugN "GetContentsBS"   *> getContentsBS
interpretGADTEffFDebug GetContentsText = logDebugN "GetContentsText" *> getContentsText
interpretGADTEffFDebug GetChar         = logAndCont =<< getChar where logAndCont c = logDebugN ("GetChar: " <> one c) $> c
interpretGADTEffFDebug GetChars        = logAndCont =<< getChars where logAndCont l = logDebugN ("GetChars: " <>     l) $> l
interpretGADTEffFDebug (PutChar c)     = logDebugN ("PutChar: " <> one c) *> putChar c
interpretGADTEffFDebug (PutChars s)    = logDebugN ("PutChars: " <>     s) *> putChars s
interpretGADTEffFDebug Flush           = logDebugN "Flush"                *> flush

interpretGADTEffF ∷ MonadEff m ⇒ GADTEffF a → m a
interpretGADTEffF GetContentsBS   = getContentsBS
interpretGADTEffF GetContentsText = getContentsText
interpretGADTEffF GetChar         = getChar
interpretGADTEffF GetChars        = getChars
interpretGADTEffF (PutChar c)     = putChar c
interpretGADTEffF (PutChars s)    = putChars s
interpretGADTEffF Flush           = flush

--------------------------------------------------------------------------------

instance MonadEff GADTEff where
  getContentsBS   = gadtGetContentsBS
  getContentsText = gadtGetContentsText
  getChar         = gadtGetChar
  getChars         = gadtGetChars
  putChar         = gadtPutChar
  putChars         = gadtPutLine
  flush           = gadtFlush

gadtGetContentsBS ∷ GADTEff LByteString
gadtGetContentsBS = liftF GetContentsBS

gadtGetContentsText ∷ GADTEff LText
gadtGetContentsText = liftF GetContentsText

gadtGetChar ∷ GADTEff Char
gadtGetChar = liftF GetChar

gadtGetChars ∷ GADTEff Text
gadtGetChars = liftF GetChars

gadtPutChar ∷ Char → GADTEff ()
gadtPutChar = liftF . PutChar

gadtPutLine ∷ Text → GADTEff ()
gadtPutLine = liftF . PutChars

gadtFlush ∷ GADTEff ()
gadtFlush = liftF Flush

--------------------------------------------------------------------------------

data GADTEffF a where
  GetContentsBS :: GADTEffF LByteString
  GetContentsText :: GADTEffF LText
  GetChar :: GADTEffF Char
  GetChars :: GADTEffF Text
  PutChar :: Char -> GADTEffF ()
  PutChars :: Text -> GADTEffF ()
  Flush :: GADTEffF ()
