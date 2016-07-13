{-|
Module      : Control.Monad.Embedding
Description : Small-step monad evaluation.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Embedding where

import Data.Functor.Identity
import Data.Functor.Compose
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Control.Monad.Trans.Writer.Strict as WS
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

newtype Embedding (f :: * -> *) (g :: * -> *) = Embedding {
    runEmbedding :: forall t . f t -> g (t, Embedding f g)
  }

execEmbedding :: Functor g => Embedding f g -> f t -> g t
execEmbedding embedding = fmap fst . runEmbedding embedding

composeEmbedding
  :: forall f g h .
     ( Functor h )
  => Embedding g h
  -> Embedding f g
  -> Embedding f h
composeEmbedding eleft eright = Embedding $ \(f :: f t) ->
  let g :: g (t, Embedding f g)
      g = runEmbedding eright f
      h :: h ((t, Embedding f g), Embedding g h)
      h = runEmbedding eleft g
      continue :: ((t, Embedding f g), Embedding g h) -> (t, Embedding f h)
      continue ((t, eright'), eleft') = (t, composeEmbedding eleft' eright')
  in  fmap continue h

idEmbedding :: ( Functor f ) => Embedding f f
idEmbedding = Embedding $ \f -> fmap (flip (,) idEmbedding) f

naturalEmbedding
  :: ( Functor g )
  => (forall t . f t -> g t)
  -> Embedding f g
naturalEmbedding trans = Embedding $ fmap (flip (,) recurse) . trans
  where
  recurse = naturalEmbedding trans

embedIdentity :: Applicative g => Embedding Identity g
embedIdentity = Embedding $ \(Identity t) -> pure (t, embedIdentity)

embedComposeL
  :: ( Functor g, Functor h )
  => Embedding f g
  -> Embedding (Compose f h) (Compose g h)
embedComposeL embedding = Embedding $ \(Compose fh) -> Compose $
  let continue = \(h, embedding') -> fmap (flip (,) (embedComposeL embedding')) h
  in  fmap continue (runEmbedding embedding fh)

embedComposeR
  :: ( Functor g, Functor h )
  => Embedding f g
  -> Embedding (Compose h f) (Compose h g)
embedComposeR embedding = Embedding $ \(Compose hf) -> Compose $
  let continue = \(t, embedding') -> (t, embedComposeR embedding')
  in  fmap (fmap continue . runEmbedding embedding) hf

embedReaderT :: Monad m => r -> Embedding (ReaderT r m) m
embedReaderT r = Embedding $ \readerT -> do
  t <- runReaderT readerT r
  pure (t, embedReaderT r)

embedStateT :: Monad m => s -> Embedding (StateT s m) m
embedStateT s = Embedding $ \stateT -> do
  (t, s') <- runStateT stateT s
  pure (t, embedStateT s')

embedWriterT :: Monad m => (w -> m ()) -> Embedding (WriterT w m) m
embedWriterT write = Embedding $ \writerT -> do
  (t, w) <- runWriterT writerT
  _ <- write w
  pure (t, embedWriterT write)

embedWriterT' :: Monad m => (w -> m ()) -> Embedding (WS.WriterT w m) m
embedWriterT' write = Embedding $ \writerT -> do
  (t, w) <- WS.runWriterT writerT
  _ <- write w
  pure (t, embedWriterT' write)


embedMaybeT :: Monad m => (forall t . m t) -> Embedding (MaybeT m) m
embedMaybeT nothing = Embedding $ \maybeT -> do
  out <- runMaybeT maybeT
  case out of
    Just t -> pure (t, embedMaybeT nothing)
    Nothing -> do
      t <- nothing
      pure (t, embedMaybeT nothing)

embedExceptT :: Monad m => (forall t . e -> m t) -> Embedding (ExceptT e m) m
embedExceptT except = Embedding $ \exceptT -> do
  out <- runExceptT exceptT
  case out of
    Right t -> pure (t, embedExceptT except)
    Left e -> do
      t <- except e
      pure (t, embedExceptT except)
