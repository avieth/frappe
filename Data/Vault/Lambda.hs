{-|
Module      : Data.Vault.Lambda
Description : A Vault for functions with common codomain but heterogeneous
              domain.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Data.Vault.Lambda (

    LambdaVault
  , DomainKey
  , newDomainKey
  , empty
  , lookup
  , insert
  , union

  ) where

import Prelude hiding (lookup)
import Unsafe.Coerce
import Data.Unique
import qualified Data.HashMap.Strict as HM

data SomeFunction t where
  SomeFunction :: (s -> t) -> SomeFunction t

instance Functor SomeFunction where
  fmap f (SomeFunction g) = SomeFunction (f . g)

-- | Phantom parameter p so that LambdaVault has the same kind as other maps,
--   and can therefore be used with MapAlgebra.
newtype LambdaVault p t = LambdaVault {
    getLambdaVault :: HM.HashMap Unique (SomeFunction t)
  }

instance Functor (LambdaVault p) where
  fmap = lvMap

newtype DomainKey s = DomainKey {
    getDomainKey :: Unique
  }

newDomainKey :: forall s . IO (DomainKey s)
newDomainKey = DomainKey <$> newUnique

empty :: forall p t . LambdaVault p t
empty = LambdaVault HM.empty

lookup :: forall p s t . DomainKey s -> LambdaVault p t -> Maybe (s -> t)
lookup d lv = case HM.lookup (getDomainKey d) (getLambdaVault lv) of
  Nothing -> Nothing
  Just (SomeFunction f) -> Just (unsafeCoerce f)

insert :: forall p s t . DomainKey s -> (s -> t) -> LambdaVault p t -> LambdaVault p t
insert d f lv = LambdaVault $
  HM.insert (getDomainKey d) (SomeFunction f) (getLambdaVault lv)

lvMap :: forall p s t . (s -> t) -> LambdaVault p s -> LambdaVault p t
lvMap f lv = LambdaVault $ fmap (fmap f) (getLambdaVault lv)

union :: forall p t . (t -> t -> t) -> LambdaVault p t -> LambdaVault p t -> LambdaVault p t
union f lvl lvr = LambdaVault $
  HM.unionWith f' (getLambdaVault lvl) (getLambdaVault lvr)
  where
  -- It's known here that the functions l, r have the same domain, for they
  -- are found at the same key in the LambdaVault map!
  f' :: SomeFunction t -> SomeFunction t -> SomeFunction t
  f' (SomeFunction (l :: s1 -> t)) (SomeFunction (r' :: s2 -> t)) =
    let r :: s1 -> t
        r = unsafeCoerce r'
    in  SomeFunction (f <$> l <*> r)
