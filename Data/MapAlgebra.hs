{-|
Module      : Data.MapAlgebra
Description : Algebra for recursive map unions.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.MapAlgebra (

    MapAlgebra(..)
  , runMapAlgebra

  ) where

import System.Mem.StableName
import qualified Data.HashMap.Lazy as HM
import GHC.Exts (Any)
import Unsafe.Coerce

type StableNames = HM.HashMap Int [Any]

insertStableName :: StableName a -> StableNames -> StableNames
insertStableName sn sns = HM.insertWith (++) (hashStableName sn) [unsafeCoerce sn] sns

unionStableNames :: StableNames -> StableNames -> StableNames
unionStableNames = HM.unionWith (++)

checkStableName :: forall a . StableName a -> StableNames -> Bool
checkStableName sn sns = case HM.lookup (hashStableName sn) sns of
  Nothing -> False
  Just lst -> elem sn (unsafeCoerce lst :: [StableName a])

data MapAlgebra m k v where
  Union :: (v -> v -> v) -> MapAlgebra m k v -> MapAlgebra m k v -> MapAlgebra m k v
  Literal :: m k v -> MapAlgebra m k v
  Fmap :: (s -> t) -> MapAlgebra m k s -> MapAlgebra m k t

instance Functor (MapAlgebra m k) where
  fmap = Fmap

-- | Produce a map from a MapAlgebra.
runMapAlgebra
  :: forall m k v .
     (forall k v . m k v)
  -> (forall v . (v -> v -> v) -> m k v -> m k v -> m k v)
  -> (forall s t . (s -> t) -> m k s -> m k t)
  -> MapAlgebra m k v
  -> IO (m k v, StableNames)
runMapAlgebra = runMapAlgebra' HM.empty

-- | The StableNames always contains the StableName of every algebra term
--   already seen.
runMapAlgebra'
  :: forall m k v .
     StableNames
  -> (forall k v . m k v)
  -> (forall v . (v -> v -> v) -> m k v -> m k v -> m k v)
  -> (forall s t . (s -> t) -> m k s -> m k t)
  -> MapAlgebra m k v
  -> IO (m k v, StableNames)
runMapAlgebra' sns empty union fmap term = do
  sn <- makeStableName term
  let sns' = insertStableName sn sns
  case term of
    Literal map -> pure (map, sns')
    Union f left right -> do
      snl <- makeStableName left
      snr <- makeStableName right
      case (checkStableName snl sns', checkStableName snr sns') of
        (False, False) -> do
          (ml, snsl) <- recurse sns' left
          (mr, snsr) <- recurse (unionStableNames sns' snsl) right
          pure (union f ml mr, snsr)
        (False, True) -> do
          (ml, snsl) <- recurse sns' left
          pure (ml, snsl)
        (True, False) -> do
          (mr, snsr) <- recurse sns' right
          pure (mr, snsr)
        (True, True) -> pure (empty, sns')
    Fmap f sub -> do
      (m, sns'') <- recurse sns' sub
      pure (fmap f m, sns'')
  where
  recurse :: forall v . StableNames -> MapAlgebra m k v -> IO (m k v, StableNames)
  recurse sns term = runMapAlgebra' sns empty union fmap term

{-
-- Does not diverge.
test = fmap fst (runMapAlgebra Map.empty Map.unionWith term)
  where
  -- Notice that we're unioning term with itself twice.
  -- runMapAlgebra will detect that and avoid divergent recursion.
  term = Union const m (Union const term (Union const m' term))
  m = Literal (Map.fromList [("Hello", False)])
  m' = Literal (Map.fromList [("World", True)])

-- Diverges.
test' = Map.unionWith const m test'
  where
  m = Map.fromList [("Hello", "World")]
-}
