{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module HelVM.HelMA.Automaton.Eff.GADTEff
  ( GADTEff
  , GADTEffF (..)
  , interpretGADTEff
  , interpretGADTEffDebug
  ) where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Logger

import           Prelude                            hiding (getLine, putLTextLn, putText, putTextLn)

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

interpretGADTEffDebug ∷ MonadLoggerEff m ⇒ GADTEff a → m a
interpretGADTEffDebug eff = runGADTEff eff interpretGADTEffFDebug

interpretGADTEff ∷ MonadEff m ⇒ GADTEff a → m a
interpretGADTEff eff = runGADTEff eff interpretGADTEffF

--------------------------------------------------------------------------------
-- Interpreter dla pojedynczych instrukcji (bez fmap/kontynuacji!)

interpretGADTEffFDebug ∷ MonadLoggerEff m ⇒ GADTEffF a → m a
interpretGADTEffFDebug GetContentsBS   = logDebugN "GetContentsBS"   *> getContentsBS
interpretGADTEffFDebug GetContentsText = logDebugN "GetContentsText" *> getContentsText
interpretGADTEffFDebug GetChar         = logAndCont =<< getChar where logAndCont c = logDebugN ("GetChar: " <> one c) $> c
interpretGADTEffFDebug GetLine         = logAndCont =<< getLine where logAndCont l = logDebugN ("GetLine: " <>     l) $> l
interpretGADTEffFDebug (PutChar c)     = logDebugN ("PutChar: " <> one c) *> putChar c
interpretGADTEffFDebug (PutText s)     = logDebugN ("PutText: " <>     s) *> putLine s
interpretGADTEffFDebug Flush           = logDebugN "Flush"                *> flush

interpretGADTEffF ∷ MonadEff m ⇒ GADTEffF a → m a
interpretGADTEffF GetContentsBS   = getContentsBS
interpretGADTEffF GetContentsText = getContentsText
interpretGADTEffF GetChar         = getChar
interpretGADTEffF GetLine         = getLine
interpretGADTEffF (PutChar c)     = putChar c
interpretGADTEffF (PutText s)     = putLine s
interpretGADTEffF Flush           = flush

--------------------------------------------------------------------------------

instance MonadEff GADTEff where
  getContentsBS   = gadtGetContentsBS
  getContentsText = gadtGetContentsText
  getChar         = gadtGetChar
  getLine         = gadtGetLine
  putChar         = gadtPutChar
  putLine         = gadtPutLine
  flush           = gadtFlush

gadtGetContentsBS ∷ GADTEff LByteString
gadtGetContentsBS = liftF GetContentsBS

gadtGetContentsText ∷ GADTEff LText
gadtGetContentsText = liftF GetContentsText

gadtGetChar ∷ GADTEff Char
gadtGetChar = liftF GetChar

gadtGetLine ∷ GADTEff Text
gadtGetLine = liftF GetLine

gadtPutChar ∷ Char → GADTEff ()
gadtPutChar = liftF . PutChar

gadtPutLine ∷ Text → GADTEff ()
gadtPutLine = liftF . PutText

gadtFlush ∷ GADTEff ()
gadtFlush = liftF Flush

--------------------------------------------------------------------------------

data GADTEffF a where
  GetContentsBS :: GADTEffF LByteString
  GetContentsText :: GADTEffF LText
  GetChar :: GADTEffF Char
  GetLine :: GADTEffF Text
  PutChar :: Char -> GADTEffF ()
  PutText :: Text -> GADTEffF ()
  Flush :: GADTEffF ()
