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
import qualified Data.HashMap.Strict as HM
import System.Mem.StableName
import Unsafe.Coerce
import GHC.Exts (Any)

type Key (f :: * -> *) t = f t
type Value (f :: * -> *) t = t

type Cache (f :: * -> *) = HM.HashMap Int [Any]

emptyCache :: Cache f
emptyCache = HM.empty

insertCache :: Key f t -> Value f t -> Cache f -> IO (Cache f)
insertCache key value cache = do
  sn <- makeStableName key
  let snh = hashStableName sn
  pure $ HM.insertWith (++) snh [unsafeCoerce (sn, value) :: Any] cache

checkCache :: forall f t . Key f t -> Cache f -> IO (Maybe (Value f t))
checkCache key cache = do
  sn <- makeStableName key
  let snh = hashStableName sn
  case HM.lookup snh cache of
    Nothing -> pure Nothing
    Just values -> pure $ checkCacheBucket sn (unsafeCoerce values)

  where

  checkCacheBucket
    :: StableName (Key f t)
    -> [(StableName (Key f t), Value f t)]
    -> Maybe (Value f t)
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
    getCached :: WriterT [r] (StateT (Cache f) f) t
  }

deriving instance Functor f => Functor (Cached r f)
deriving instance Monad f => Applicative (Cached r f)
deriving instance Monad f => Monad (Cached r f)

runCached
  :: forall r f t .
     ( Monad f )
  => Cached r f t
  -> WriterT [r] f t
runCached (Cached (WriterT stateT)) = WriterT (evalStateT stateT emptyCache)

transCached
  :: forall r f g t .
     (forall t . f t -> g t)
  -> Cached r f t
  -> Cached r g t
transCached trans (Cached (WriterT (StateT mkfterm))) =
  Cached (WriterT (StateT (fmap trans mkfterm)))

cached
  :: forall r f t.
     ( Monad f )
  => (forall t . IO t -> f t)
  -> WriterT [r] f t
  -> Cached r f t
cached liftIO term = Cached $ WriterT $ do
  cache <- get
  value :: Maybe t <- lift . liftIO $ checkCache term cache
  case value of
    Just t -> pure (t, [])
    Nothing -> do
      out :: (t, [r]) <- lift (runWriterT term)
      cache' <- lift . liftIO $ insertCache term (fst out) cache
      _ <- put cache'
      pure out

withResidues
  :: ( Monad f )
  => Cached r f t
  -> Cached s f (t, [r])
withResidues (Cached (WriterT term)) = Cached (lift term)
