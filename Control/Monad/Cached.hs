{-|
Module      : Control.Monad.Cached
Description : Definition of a peculiar cached writer monad.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Control.Monad.Cached (

    Cached
  , runCached
  , cached
  , transCached
  , withResidues

  ) where

import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import System.Mem.StableName
import Unsafe.Coerce
import GHC.Exts (Any)

type Key r (f :: * -> *) t = WriterT [r] f t
type Value r (f :: * -> *) t = (t, [r])

type Cache r (f :: * -> *) = HM.HashMap Int [Any]

fromAny :: Any -> t
fromAny = unsafeCoerce

toAny :: t -> Any
toAny = unsafeCoerce

emptyCache :: Cache r f
emptyCache = HM.empty

insertCache :: Key r f t -> Value r f t -> Cache r f -> IO (Cache r f)
insertCache key value cache = do
  sn <- makeStableName key
  let snh = hashStableName sn
  pure $ HM.insertWith (++) snh [unsafeCoerce (sn, value) :: Any] cache

checkCache :: forall r f t . Key r f t -> Cache r f -> IO (Maybe (Value r f t))
checkCache key cache = do
  sn <- makeStableName key
  let snh = hashStableName sn
  case HM.lookup snh cache of
    Nothing -> pure Nothing
    Just values -> pure $ checkCacheBucket sn (unsafeCoerce values)

  where

  checkCacheBucket
    :: StableName (Key r f t)
    -> [(StableName (Key r f t), Value r f t)]
    -> Maybe (Value r f t)
  checkCacheBucket sn lst = case lst of
    [] -> Nothing
    (sn', v) : rest ->
      if sn' `eqStableName` sn then Just v else checkCacheBucket sn rest

-- | Tailor-made for use in defining events.
--   Cached r f t consists of WriterT [r] f terms which are cached using
--   their stable names; the same term (on the heap) will have its effects
--   realized at most once.
--   This makes Cached r f *not* a monad, but it gives the desired semantics
--   when wielded appropriately to define events.
newtype Cached (r :: *) (f :: * -> *) (t :: *) = Cached {
    getCached :: StateT (Cache r f) (WriterT [r] f) t
  }

deriving instance Functor f => Functor (Cached r f)
deriving instance Monad f => Applicative (Cached r f)
deriving instance Monad f => Monad (Cached r f)

runCached
  :: forall r f t .
     ( Monad f )
  => Cached r f t
  -> WriterT [r] f t
runCached (Cached stateT) = evalStateT stateT emptyCache

transCached
  :: forall r f g t .
     (forall t . f t -> g t)
  -> Cached r f t
  -> Cached r g t
transCached trans (Cached (StateT mkwriterterm)) =
  Cached (StateT (fmap transWriter mkwriterterm))
  where
  transWriter (WriterT fterm) = WriterT (trans fterm)

cached
  :: forall r f t.
     ( MonadIO f )
  => WriterT [r] f t
  -> Cached r f t
cached term = Cached $ do
  cache <- get
  value :: Maybe (t, [r]) <- liftIO $ checkCache term cache
  case value of
    -- Cache hit. Lift t in but ignore the writer output, as we don't want
    -- to make duplicate writes.
    Just (t, _) -> lift (pure t)
    -- Cache miss. Run the term and cache it.
    Nothing -> do
      out :: (t, [r]) <- lift (listen term)
      cache' <- liftIO $ insertCache term out cache
      _ <- put cache'
      lift (pure (fst out))

-- | Include a complete cached computation and obtain its writer output.
withResidues
  :: forall r s f t .
     ( MonadIO f )
  => Cached r f t
  -> Cached s f (t, [r])
withResidues (Cached statet) = Cached $ do
  cache <- get
  let fterm :: f ((t, Cache r f), [r])
      fterm = runWriterT (runStateT statet cache)
  ((t, cache'), rs) <- lift (lift fterm)
  _ <- put cache'
  pure (t, rs)
