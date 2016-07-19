{-|
Module      : Reactive.Frappe
Description : Functor-centric reactive programming.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Reactive.Frappe (

    Event
  , never
  , now
  , immediately
  , applyEvents
  , (>*<)
  , (>*)
  , (*<)
  , unionEvents
  , withEvent
  , repeatIndefinitely
  , embedEvent
  , transEvent
  , emit

  , factorEvent
  , nextSemigroup

  , Delay
  , indefiniteDelay
  , delayed
  , andThen
  , switchDelay
  , unionDelays
  , mergeDelays
  , applyDelays
  , withDelay
  , embedDelay
  , transDelay

  , Accumulator
  , accumulator
  , accumulate
  , accumulateAll

  , Stepper(..)
  , stepper
  , initial
  , changes
  , applyStepper
  , (<@>)

  , React
  , sync
  , async
  , asyncOS
  , primEvent

  , Reactive
  , reactive
  , runReactive

  , Network
  , reactimate
  , runNetwork

  ) where

import Control.Applicative
import Control.Monad (ap, join)
import Control.Exception
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad.Embedding
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Free.Church
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State
import Control.Monad.Cached
import Data.Void
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Bifunctor (bimap)
import Data.Semigroup
import Data.List.NonEmpty
import qualified Data.Vault.Lambda as LVault
import Data.MapAlgebra

-- |
-- = Stepper (classical behavior)

newtype Stepper a r f t = Stepper {
    getStepper :: (t, Delay a r f t)
  }

instance ( Monoid r ) => Functor (Stepper a r f) where
  fmap f ~(Stepper (initial, next)) = Stepper (f initial, fmap f next)

instance ( Monoid r ) => Applicative (Stepper a r f) where
  pure t = Stepper (t, indefiniteDelay)
  ~(Stepper (f, delayf)) <*> ~(Stepper (x, delayx)) = Stepper $
    (y, fmap snd (unionDelays unioner delayf' delayx'))
    where
    unioner ((f, _), _) ((_, x), _) = ((f, x), f x)
    delayf' = fmap (\f -> ((f, x), f x)) delayf
    delayx' = fmap (\x -> ((f, x), f x)) delayx
    y = f x

stepper :: ( ) => t -> Delay a r f t -> Stepper a r f t
stepper t delay = Stepper (t, delay)

initial :: Stepper a r f t -> t
initial = fst . getStepper

changes :: Stepper a r f t -> Delay a r f t
changes = snd . getStepper

applyStepper
  :: forall a r f s t .
     ( Monoid r, Functor f )
  => Stepper a r f (s -> t)
  -> Delay a r f s
  -> Delay a r f t
applyStepper ~(Stepper (f, step)) delay =
  mergeDelays merger step delay
  where
  merger
    :: Either (s -> t) (Delay a r f (s -> t))
    -> Either s (Delay a r f s)
    -> Either t (Delay a r f t)
  merger cf cx = case (cf, cx) of
    (Left f', Left x) -> Left (f' x)
    (Left f', Right x) -> Right (applyStepper (stepper f' step) x)
    (Right _, Left x) -> Left (f x)
    (Right f', Right x) -> Right (applyStepper (stepper f f') x)

infixl 4 <@>
(<@>)
  :: ( Monoid r, Functor f )
  => Stepper a r f (s -> t)
  -> Delay a r f s
  -> Delay a r f t
(<@>) = applyStepper

-- | Some @f@ with IO mixed in. @SyncF f@ is a functor whenever @f@ is a
--   functor.
--   IO is there for the benefit of Cached. It means that SyncF f is MonadIO, so
--   we can create StableNames in it.
type SyncF f = FT f IO

liftSyncF :: ( Functor f ) => f t -> SyncF f t
liftSyncF = liftF

-- | A SyncF can be injected into IO by using an Embedding.
embedSyncF
  :: forall f t .
     ( Functor f )
  => SyncF f t
  -> StateT (Embedding f IO) IO t
embedSyncF = iterTM embed
  where
  embed :: forall t . f (StateT (Embedding f IO) IO t)
        -> StateT (Embedding f IO) IO t
  embed term = do
    embedding <- get
    (t, embedding') <- lift (runEmbedding embedding term)
    _ <- put embedding'
    t

-- | Any Embedding f g determines an Embedding (SyncF f) (SyncF g)
syncFEmbedding
  :: forall f g .
     ( Functor f, Functor g )
  => Embedding f g
  -> Embedding (SyncF f) (SyncF g)
syncFEmbedding embedding = Embedding $ \syncf -> do
  (t, embedding') <- runStateT (runSyncStateT (iterTM embed syncf)) embedding
  pure (t, syncFEmbedding embedding')
  where
  embed :: forall t . f (SyncStateT f g IO t)
        -> SyncStateT f g IO t
  embed term = SyncStateT $ do
    embedding <- get
    (t, embedding') <- liftF (runEmbedding embedding term)
    _ <- put embedding'
    runSyncStateT t

-- | Used to define syncFEmbedding. It's a target for an iterTM on a SyncF.
newtype SyncStateT f g h t = SyncStateT {
    runSyncStateT :: StateT (Embedding f g) (FT g h) t
  }

deriving instance Functor (SyncStateT f g h)
deriving instance Applicative (SyncStateT f g h)
deriving instance Monad (SyncStateT f g h)
instance MonadTrans (SyncStateT f g) where
  lift = SyncStateT . lift . lift

-- | Stores functions from pulses (primitives events) to some value.
--   Running an event (primEvent callback) will check the pulses of the
--   top-level event using its own domain key. If it's present, then it will
--   recover the f computation to run to produce a value and the next event.
--
--   A MapAlgebra is used so that we can union a Pulses p r f t with itself
--   without diverging.
type Pulses (p :: *) (a :: *) (r :: *) (f :: * -> *) (t :: *) =
  MapAlgebra LVault.LambdaVault p (Event a r f t)

-- | An Event r f t which will appear at some time in the future.
newtype Delay (a :: *) (r :: *) (f :: * -> *) (t :: *) = Delay {
    getDelay :: forall p . Pulses p a r f t
  }

instance ( Monoid r ) => Functor (Delay a r f) where
  fmap = mapDelay

instance ( Monoid r, Semigroup t ) => Semigroup (Delay a r f t) where
  (<>) = unionDelays (<>)

instance ( Monoid r, Semigroup t ) => Monoid (Delay a r f t) where
  mempty = indefiniteDelay
  mappend = (<>)

delayed :: ( Monoid r ) => Delay a r f t -> Event a r f t
delayed = Event . cached . pure . Right

-- | A Delay which never ends.
indefiniteDelay :: Delay a r f t
indefiniteDelay = Delay (Literal LVault.empty)

-- | Continue a Delay.
andThen
  :: ( Monoid r, Functor f )
  => Delay a r f s
  -> (s -> Event a r f t)
  -> Delay a r f t
andThen delay k = Delay $ Fmap (>>= k) (getDelay delay)

-- | Some f-computation which may or may not immediately yield a value.
newtype Event (a :: *) (r :: *) (f :: * -> *) (t :: *) = Event {
    unEvent :: Cached r (SyncF f) (Either t (Delay a r f t))
  }

instance ( Monoid r ) => Functor (Event a r f) where
  fmap = mapEvent

instance ( Monoid r, Functor f ) => Applicative (Event a r f) where
  pure = pureEvent
  -- NB the applicative and monad instances for Event r f are consistent; we
  -- do *not* use (<*>) = applyEvents as that function introduces
  -- concurrency.
  (<*>) = ap

instance ( Monoid r, Functor f ) => Monad (Event a r f) where
  return = pure
  ev >>= k = switchEvent (mapEvent k ev)

instance ( Monoid r, Functor f ) => Alternative (Event a r f) where
  empty = never
  left <|> right = unionEvents const left right

instance ( Monoid r ) => MonadTrans (Event a r) where
  lift = Event . cached . lift . liftF . fmap Left

instance ( Monoid r, Semigroup t ) => Semigroup (Event a r f t) where
  (<>) = unionEvents (<>)

instance ( Monoid r, Semigroup t ) => Monoid (Event a r f t) where
  mempty = never
  mappend = (<>)

-- | An Event which never happens.
never :: ( Monoid r ) => Event a r f t
never = Event . cached $ pure (Right indefiniteDelay)

-- | Like lift but better, because it doesn't have the Monad f constraint.
now :: ( Monoid r, Functor f ) => f t -> Event a r f t
now term = immediate (lift (liftSyncF term))

-- | Put out some data on the Event's side-channel.
emit :: ( Monoid r ) => r -> Event a r f ()
emit r = immediate (tell r)

immediate
  :: forall a r f t .
     ( Monoid r )
  => WriterT r (SyncF f) t
  -> Event a r f t
immediate term =
  let term' :: WriterT r (SyncF f) (Either t (Delay a r f t))
      term' = fmap Left term
  in  Event (cached term')

-- | Repeat an Event again and again, realizing its effects but never a value.
repeatIndefinitely :: ( Monoid r, Functor f ) => Event a r f t -> Event a r f x
repeatIndefinitely event = let event' = event >> event' in event'

-- | An Event which immediately yields some value.
pureEvent
  :: forall a r f t .
     ( Monoid r )
  => t
  -> Event a r f t
pureEvent t = Event . cached $ pure (Left t)

-- | Like Applicative pure but inside f.
immediately
  :: forall a r f t .
     ( Monoid r, Functor f )
  => f t
  -> Event a r f t
immediately term = immediate (lift (liftSyncF term))

mapEvent
  :: forall a r f s t .
     ( Monoid r )
  => (s -> t)
  -> Event a r f s
  -> Event a r f t
mapEvent f event = Event $ fmap (bimap f (mapDelay f)) (unEvent event)

mapDelay
  :: forall a r f s t .
     ( Monoid r )
  => (s -> t)
  -> Delay a r f s
  -> Delay a r f t
mapDelay f delay = Delay pulses
  where
  pulses :: Pulses p a r f t
  pulses = Fmap (mapEvent f) (getDelay delay)

changeEvent
  :: forall a r q f t .
     ( Monoid q, Functor f )
  => (r -> q)
  -> Event a r f t
  -> Event a q f t
changeEvent change event = Event $ do
  choice <- changeCached change (unEvent event)
  case choice of
    Left t -> pure (Left t)
    Right delay -> pure (Right (changeDelay change delay))

changeDelay
  :: forall a r q f t .
     ( Monoid q, Functor f )
  => (r -> q)
  -> Delay a r f t
  -> Delay a q f t
changeDelay change delay = Delay pulses
  where
  pulses :: forall p . Pulses p a q f t
  pulses = fmap (changeEvent change) (getDelay delay)

-- | Use a natural transformation to bring some Event to another functor.
transEvent
  :: forall a r f g t .
     ( Functor f, Functor g, Monoid r )
  => (forall t . f t -> g t)
  -> Event a r f t
  -> Event a r g t
transEvent trans = embedEvent (naturalEmbedding trans)

-- | Use an embedding to bring some Event to another functor.
--   @transEvent@ is a special case of this.
embedEvent
  :: forall a r f g t .
     ( Functor f, Functor g, Monoid r )
  => Embedding f g
  -> Event a r f t
  -> Event a r g t
embedEvent embedding = embedEvent_ embedding''
  where
  embedding' :: Embedding (SyncF f) (SyncF g)
  embedding' = syncFEmbedding embedding
  embedding'' :: Embedding (Cached r (SyncF f)) (Cached r (SyncF g))
  embedding'' = cachedEmbedding embedding'

  embedEvent_
    :: Embedding (Cached r (SyncF f)) (Cached r (SyncF g))
    -> Event a r f t
    -> Event a r g t
  embedEvent_ embedding event = Event $ do
    (choice, embedding') <- runEmbedding embedding (unEvent event)
    case choice of
      Left done -> pure (Left done)
      Right delay -> pure (Right (Delay pulses))
        where
        pulses :: forall p . Pulses p a r g t
        pulses = Fmap (embedEvent_ embedding') (getDelay delay)

transDelay
  :: forall a r f g t .
     ( Functor f, Functor g, Monoid r )
  => (forall t . f t -> g t)
  -> Delay a r f t
  -> Delay a r g t
transDelay trans = embedDelay (naturalEmbedding trans)

embedDelay
  :: forall a r f g t .
     ( Functor f, Functor g, Monoid r )
  => Embedding f g
  -> Delay a r f t
  -> Delay a r g t
embedDelay embedding delay = Delay pulses
  where
  pulses :: forall p . Pulses p a r g t
  pulses = fmap (embedEvent embedding) (getDelay delay)

-- | Monadic join for Events.
switchEvent
  :: forall a r f t .
     ( Monoid r, Functor f )
  => Event a r f (Event a r f t)
  -> Event a r f t
switchEvent event = Event $ do
  choice <- unEvent event
  case choice of
    Left (Event term') -> term'
    Right delay -> pure (Right (switchDelay delay))

switchDelay
  :: forall a r f t .
     ( Monoid r, Functor f )
  => Delay a r f (Event a r f t)
  -> Delay a r f t
switchDelay ~(Delay pulses) = Delay pulses'
  where
  pulses' :: Pulses p a r f t
  pulses' = fmap switchEvent pulses

infixl 4 >*<
-- | Parallel event application: effects from both Events are realized
--   concurrently but the derived Event fires simultaneously with the last
--   of these two Events to fire.
--   @applyEvents@ is a synonym.
(>*<)
  :: forall a r f s t .
     ( Monoid r )
  => Event a r f (s -> t)
  -> Event a r f s
  -> Event a r f t
(>*<) = applyEvents

infixl 4 >*
(>*)
  :: forall a r f s t .
     ( Monoid r )
  => Event a r f s
  -> Event a r f t
  -> Event a r f s
(>*) = applyEvents . fmap const

infixl 4 *<
(*<)
  :: forall a r f s t .
     ( Monoid r )
  => Event a r f s
  -> Event a r f t
  -> Event a r f t
(*<) = applyEvents . fmap (flip const)

-- | Include the effects of some event in some never-ending event.
withEvent
  :: forall a r f .
     ( Monoid r )
  => Event a r f ()
  -> Event a r f Void
  -> Event a r f Void
withEvent evu evv = Event $ do
  mu :: Either () (Delay a r f ()) <- unEvent evu
  mv :: Either Void (Delay a r f Void) <- unEvent evv
  pure $ case (mu, mv) of

    (_, Left void) -> absurd void

    (Left _, Right delay) -> Right delay

    (Right delayu, Right delayv) -> Right (withDelay delayu delayv)

withDelay
  :: forall a r f .
     ( Monoid r )
  => Delay a r f ()
  -> Delay a r f Void
  -> Delay a r f Void
withDelay dlu dlv = Delay pulses

  where

  pulsesu :: forall p . Pulses p a r f ()
  pulsesu = getDelay dlu

  pulsesv :: forall p . Pulses p a r f Void
  pulsesv = getDelay dlv

  decomposed :: MapAlgebra LVault.LambdaVault p (PulseDecomp (Event a r f ()) (Event a r f Void))
  decomposed = decomposePulses pulsesu pulsesv

  pulses :: Pulses p a r f Void
  pulses = recomposePulses handleLeft handleRight handleBoth decomposed

  handleLeft :: Event a r f () -> Event a r f Void
  handleLeft event = Event $ do
    choice <- unEvent event
    case choice of
      Left () -> pure (Right dlv)
      Right delay -> pure (Right (withDelay delay dlv))

  handleRight :: Event a r f Void -> Event a r f Void
  handleRight event = Event $ do
    choice <- unEvent event
    case choice of
      Left void -> absurd void
      Right delay -> pure (Right (withDelay dlu delay))

  handleBoth :: Event a r f () -> Event a r f Void -> Event a r f Void
  handleBoth = withEvent

-- | Parallel Event application. @(>*<)@ is a synonym.
applyEvents
  :: forall a r f s t .
     ( Monoid r )
  => Event a r f (s -> t)
  -> Event a r f s
  -> Event a r f t
applyEvents evf evx = Event $ do
  mf :: Either (s -> t) (Delay a r f (s -> t)) <- unEvent evf
  mx :: Either s (Delay a r f s) <- unEvent evx
  pure $ case (mf, mx) of

    (Left f, Left x) -> Left (f x)

    (Left f, Right (Delay pulses)) ->
      Right (Delay (fmap (mapEvent f) pulses))

    (Right (Delay pulses), Left x) ->
      Right (Delay (fmap (mapEvent (flip ($) x)) pulses))

    (Right df, Right dx) -> Right (applyDelays df dx)

-- | Like @applyEvents@ aka @(>*<)@ but for Delays.
applyDelays
  :: forall a r f s t .
     ( Monoid r )
  => Delay a r f (s -> t)
  -> Delay a r f s
  -> Delay a r f t
applyDelays df dx = Delay pulses'
  where

  pulsesf :: forall p . Pulses p a r f (s -> t)
  pulsesf = getDelay df

  pulsesx :: forall p . Pulses p a r f s
  pulsesx = getDelay dx

  decomposed :: MapAlgebra LVault.LambdaVault p (PulseDecomp (Event a r f (s -> t)) (Event a r f s))
  decomposed = decomposePulses pulsesf pulsesx

  pulses' :: Pulses p a r f t
  pulses' = recomposePulses handleLeft handleRight handleBoth decomposed

  handleLeft :: Event a r f (s -> t) -> Event a r f t
  handleLeft event = Event $ do
    choice <- (unEvent event)
    case choice of
      Left f -> pure (Right (Delay (fmap (mapEvent f) pulsesx)))
      Right delay -> pure (Right (applyDelays delay dx))

  handleRight :: Event a r f s -> Event a r f t
  handleRight event = Event $ do
    choice <- (unEvent event)
    case choice of
      Left x -> pure (Right (Delay (fmap (mapEvent (flip ($) x)) pulsesf)))
      Right delay -> pure (Right (applyDelays df delay))

  handleBoth :: Event a r f (s -> t) -> Event a r f s -> Event a r f t
  handleBoth = applyEvents

-- | Take the first to fire of the two Events. If they fire simultaneously,
--   use a disambiguating function.
unionEvents
  :: forall a r f t .
     ( Monoid r )
  => (t -> t -> t)
  -> Event a r f t
  -> Event a r f t
  -> Event a r f t
unionEvents disambiguate evl evr = Event $ do
  -- We don't cache this part of the computation. That's fine, as the
  -- computations inside evl, evr will be appropriately cached, and what we
  -- do here introduces no extra side-effects.
  l :: Either t (Delay a r f t) <- unEvent evl
  r :: Either t (Delay a r f t) <- unEvent evr
  pure $ case (l, r) of
    (Left tl, Left tr) -> Left (disambiguate tl tr)
    (Left tl, Right _) -> Left tl
    (Right _, Left tr) -> Left tr
    (Right dl, Right dr) -> Right (unionDelays disambiguate dl dr)

-- | Take the first to fire of the two Delays. If they fire simultaneously,
--   use a disambiguating function.
unionDelays
  :: forall a r f t .
     ( Monoid r )
  => (t -> t -> t)
  -> Delay a r f t
  -> Delay a r f t
  -> Delay a r f t
unionDelays disambiguate = mergeDelays merger
  where
  merger :: Either t (Delay a r f t) -> Either t (Delay a r f t) -> Either t (Delay a r f t)
  merger left right = case (left, right) of
    -- Both delays finish simultaneously.
    (Left tl, Left tr) -> Left (disambiguate tl tr)
    -- The left delay has finished but the right has not.
    (Left tl, Right _) -> Left tl
    -- The right delay has finished but the left has not.
    (Right _, Left tr) -> Left tr
    -- Some delay has fired but neither has finished.
    (Right dl, Right dr) -> Right (unionDelays disambiguate dl dr)

-- | Combine two delays by indicating how to continue when either or both of
--   them fire. The merger function is called with the final value or the
--   latest continuation for both delays.
--   Check out @unionDelays@ and @applyStepper@ for examples of use.
mergeDelays
  :: forall a x f s t r .
     ( Monoid x )
  => (Either s (Delay a x f s) -> Either t (Delay a x f t) -> Either r (Delay a x f r))
  -> Delay a x f s
  -> Delay a x f t
  -> Delay a x f r
mergeDelays merger delays delayt = Delay pulses

  where

  decomposed :: MapAlgebra LVault.LambdaVault p (PulseDecomp (Event a x f s) (Event a x f t))
  decomposed = decomposePulses (getDelay delays) (getDelay delayt)

  pulses :: Pulses p a x f r
  pulses = recomposePulses handleLeft handleRight handleBoth decomposed

  handleLeft :: Event a x f s -> Event a x f r
  handleLeft event = Event $ do
    choice <- unEvent event
    pure (merger choice (Right delayt))

  handleRight :: Event a x f t -> Event a x f r
  handleRight event = Event $ do
    choice <- unEvent event
    pure (merger (Right delays) choice)

  handleBoth :: Event a x f s -> Event a x f t -> Event a x f r
  handleBoth events eventt = Event $ do
    choices <- unEvent events
    choicet <- unEvent eventt
    pure (merger choices choicet)

data PulseDecomp l r where
  PDLeft :: l -> PulseDecomp l r
  PDRight :: r -> PulseDecomp l r
  PDBoth :: l -> r -> PulseDecomp l r

decomposePulses
  :: Pulses p a x f l
  -> Pulses p a x f r
  -> MapAlgebra LVault.LambdaVault p (PulseDecomp (Event a x f l) (Event a x f r))
decomposePulses lefts rights = Union unioner lefts' rights'
  where
  lefts' = Fmap PDLeft lefts
  rights' = Fmap PDRight rights
  unioner l r = case (l, r) of
    (PDLeft l', PDRight r') -> PDBoth l' r'
    -- Shame. How to prove this to GHC?
    _ -> error "impossible"

recomposePulses
  :: (Event a x f l -> Event a q f t)
  -> (Event a x f r -> Event a q f t)
  -> (Event a x f l -> Event a x f r -> Event a q f t)
  -> MapAlgebra LVault.LambdaVault p (PulseDecomp (Event a x f l) (Event a x f r))
  -> Pulses p a q f t
recomposePulses left right both = Fmap recompose
  where
  recompose term = case term of
    PDLeft l -> left l
    PDRight r -> right r
    PDBoth l r -> both l r

factorEvent
  :: forall a r q f s t .
     ( Monoid q )
  => (r -> Either (Event a r f s) s -> Event a q f t)
  -> Event a r f s
  -> Event a q f t
factorEvent k event = Event $ do
  (choice, rs) <- withResidues (unEvent event)
  case choice of
    Left s -> unEvent (k rs (Right s))
    Right (Delay pulses) -> pure (Right (Delay (fmap (k rs . Left) pulses)))

nextSemigroup
  :: ( Functor f, Semigroup r, Monoid q )
  => Event a [r] f t
  -> Event a q f r
nextSemigroup = factorEvent $ \rs choice -> case nonEmpty rs of
  Just ne -> pure (sconcat ne)
  Nothing -> case choice of
    Left event' -> nextSemigroup event'
    Right _ -> never

newtype Accumulator a r f t = Accumulator {
    getAccumulator :: Event a r f (t, Maybe (Accumulator a r f t))
  }

accumulate
  :: forall a r f s t .
     ( Functor f, Monoid r )
  => (s -> Maybe (Accumulator a r f s) -> Event a r f t)
  -> Accumulator a r f s
  -> Event a r f t
accumulate k acc = do
  (t, next) <- getAccumulator acc
  k t next

-- | Take the final value of an accumulator. It's done when the accumulator
--   input event is done.
accumulateAll
  :: ( Functor f, Monoid r )
  => Accumulator a r f t
  -> Event a r f t
accumulateAll = accumulate $ \t next -> case next of
  Nothing -> pure t
  Just acc -> accumulateAll acc

-- | Repeatedly apply the side-channel output of some event to an initial
--   value.
accumulator
  :: forall a f r t anything .
     ( Functor f, Monoid r )
  => t
  -> Event a (Option (Last (t -> t))) f anything
  -> Accumulator a r f t
accumulator acc event = Accumulator $ flip factorEvent event $ \r choice -> case r of
  Option Nothing -> case choice of
    Right _ -> pure (acc, Nothing)
    Left event' -> getAccumulator (accumulator acc event')
  Option (Just (Last f)) -> case choice of
    Right _ -> pure (f acc, Nothing)
    Left event' -> pure (acc', Just next)
      where
      acc' :: t
      acc' = f acc
      next :: Accumulator a r f t
      next = accumulator acc' event'

-- | A @NowEnv a r f t@ contains data necessary to pulse an event network.
data NowEnv (a :: *) (r :: *) (f :: * -> *) (t :: *) = NowEnv {
    nowChan :: Chan (r, Maybe t) -- ^ Dump outputs here
  , nowEval :: MVar (Delay a r f t, Embedding f IO)
    -- ^ The current continuation (Delay) and embedding.
  }

-- | IO with a @NowEnv r f q@. Primitive event handlers are created in @Now@,
--   so that a trigger function can use the @NowEnv@ to pulse the network.
newtype Now (a :: *) (r :: *) (q :: *) (f :: * -> *) (t :: *) = Now {
    runNow :: ReaderT (NowEnv a r f q) IO t
  }

deriving instance Functor (Now a r q f)
deriving instance Applicative (Now a r q f)
deriving instance Monad (Now a r q f)

-- | A @Now r q f t@ term which is almost free in @r@ and @q@: the former
--   must be a @Monoid@ and the latter a @Functor@.
--
--   It can do IO via @sync@ and @async@, and can create primitive @Delay@s.
newtype React (a :: *) (t :: *) = React {
    runReact :: forall r q f . ( Monoid r, Functor f ) => Now a r q f t
  }

deriving instance Functor (React a)

instance Applicative (React a) where
  pure x = React $ pure x
  React mf <*> React mx = React $ mf <*> mx

instance Monad (React a) where
  return = pure
  React x >>= k = React $ x >>= runReact . k

instance MonadIO (React a) where
  liftIO = sync

embedNow :: NowEnv a r f q -> Embedding (Now a r q f) IO
embedNow nowEnv =
  let embedding = Embedding $ \(Now reader) -> do
        out <- runReaderT reader nowEnv
        pure (out, embedding)
  in  embedding

embedReact
  :: forall a r q f .
     ( Monoid r, Functor f )
  => Embedding (React a) (Now a r q f)
embedReact = Embedding $ \react -> fmap (flip (,) embedReact) (runReact react)

syncNow :: IO t -> Now a r q f t
syncNow io = Now $ ReaderT $ \_ -> io

-- | Do synchronous IO. You probably don't want to do anything that might be
--   long-running.
sync :: IO t -> React a t
sync io = React $ syncNow io

asyncNow
  :: ( Monoid r, Monoid x, Functor f )
  => IO t
  -> Now a r q f (Delay a x g t)
asyncNow io = do
  (ev, cb) <- primEventNow
  _ <- syncNow $ do result <- Async.async (io >>= cb)
                    Async.link result
  pure ev

-- | Do asynchronous IO. The @Delay@ fires when it's done.
async :: ( Monoid x ) => IO t -> React a (Delay a x f t)
async io = React $ asyncNow io

asyncOSNow
  :: ( Monoid r, Monoid x, Functor f )
  => IO t
  -> Now a r q f (Delay a x g t)
asyncOSNow io = do
  (ev, cb) <- primEventNow
  _ <- syncNow $ do result <- Async.asyncBound (io >>= cb)
                    Async.link result
  pure ev

-- | Do asynchronous IO in an OS thread. The @Delay@ fires when it's done.
asyncOS
  :: ( Monoid x )
  => IO t
  -> React a (Delay a x f t)
asyncOS io = React $ asyncOSNow io

-- | A primitive event and a function to fire it.
primEventNow
  :: forall a r q x f g t .
     ( Monoid r, Monoid x, Functor f )
  => Now a r q f (Delay a x g t, t -> IO ())
primEventNow = Now $ ReaderT $ \nowEnv -> do
  domainKey :: LVault.DomainKey t <- LVault.newDomainKey
  let pulse :: t -> Event a x g t
      pulse = \t -> Event (cached (pure (Left t)))
      thisPulses :: Pulses p a x g t
      thisPulses = Literal (LVault.insert domainKey pulse LVault.empty)
      delay :: Delay a x g t
      delay = Delay thisPulses
  -- When the event fires, we grab the top-level event and check whether
  -- this event determines a computation for it. If it does, run it to
  -- come up with output and the next event.
  let cb = \t -> do stuff@(Delay pulses, embedding)
                      <- takeMVar (nowEval nowEnv)
                    (pulsesMap, _) <- runMapAlgebra LVault.empty LVault.union fmap pulses
                    case LVault.lookup domainKey pulsesMap of
                      Nothing -> do
                        --trace ("Squelching unobserved event") (pure ())
                        putMVar (nowEval nowEnv) stuff
                      Just (computation :: t -> Event a r f q) -> do
                        let Event cached = computation t
                            syncf :: SyncF f (Either q (Delay a r f q), r)
                            syncf = runWriterT (runCached cached)
                            statet :: StateT (Embedding f IO) IO (Either q (Delay a r f q), r)
                            statet = embedSyncF syncf
                        ((choice' :: Either q (Delay a r f q), rs), embedding')
                          <- runStateT statet embedding
                        _ <- case choice' of
                          Left done -> do
                            _ <- writeChan (nowChan nowEnv) (rs, Just done)
                            pure ()
                          Right delay' -> do
                            _ <- putMVar (nowEval nowEnv) (delay', embedding')
                            _ <- writeChan (nowChan nowEnv) (rs, Nothing)
                            pure ()
                        pure ()
  pure (delay, cb)

-- | Get a @Delay@ and a function to pulse it.
primEvent :: ( Monoid x ) => React a (Delay a x f t, t -> IO ())
primEvent = React primEventNow

-- | A React term which yields an Event, such that the Event cannot contain
--   an Event made in that same React.
--
--   Note that if f has React built-in (perhaps it is React, or is a transformer
--   or Compose over React) then the Reactive r f t won't be usable in
--   reactimate, for (due to our choice of exports from this module) there is
--   no way for the programmer to create an Embedding f IO.
data Reactive (r :: *) (f :: * -> *) (t :: *) = Reactive {
    runReactive :: forall (a :: *) . React a (Event a r (Compose f (React a)) t)
  }

instance ( Monoid r ) => Functor (Reactive r f) where
  fmap f (Reactive react) = Reactive $ (fmap . fmap) f react

reactive :: (forall a . React a (Event a r (Compose f (React a)) t)) -> Reactive r f t
reactive = Reactive

-- | Represents an active network.
newtype Network r q = Network (Chan (r, Maybe q))

-- | Set up a Network for a Reactive computation. The functor parameter of
--   the Reactive must not be capable of doing React computations, else it
--   will be impossible to embed it into Identity (without unsafe features,
--   which would almost surely cause a crash).
reactimate
  :: forall a r q f .
     ( Monoid r, Functor f )
  => Embedding f Identity
  -> f (Reactive r f q)
  -> IO (Network r q)
reactimate embedding fterm = do
  chan <- newChan
  mvar <- newEmptyMVar
  let nowEnv = NowEnv chan mvar
  let Identity (reactive, embedding') = runEmbedding embedding fterm
  case reactive of
    (Reactive (react :: forall a . React a (Event a r (Compose f (React a)) q))) -> do
      let nowTerm :: Now a r q (Compose f (React a)) (Event a r (Compose f (React a)) q)
          nowTerm = runReact react
      Event cached :: Event a r (Compose f (React a)) q
        <- runReaderT (runNow nowTerm) nowEnv
      let syncf :: SyncF (Compose f (React a)) (Either q (Delay a r (Compose f (React a)) q), r)
          syncf = runWriterT (runCached cached)
          statet :: StateT (Embedding (Compose f (React a)) IO) IO (Either q (Delay a r (Compose f (React a)) q), r)
          statet = embedSyncF syncf
      let embedReactIO :: Embedding (React a) IO
          embedReactIO = embedNow nowEnv
                         `composeEmbedding`
                         embedReact
          -- An embedding of f into IO through the embedding into Identity.
          embedFIO :: Embedding f IO
          embedFIO = embedIdentity `composeEmbedding` embedding'
          -- Embed the right-hand-side of the compose (React a) into IO, then
          -- embed the left-hand-side (f) into IO and merge.
          embedding'' :: Embedding (Compose f (React a)) IO
          embedding'' = embedComposeInR embedFIO
                        `composeEmbedding`
                        embedComposeR embedReactIO
      ((choice, rs), embedding'') <- runStateT statet embedding''
      case choice of
        Left done -> do
          writeChan chan (rs, Just done)
        Right delay -> do
          _ <- putMVar mvar (delay, embedding'')
          writeChan chan (rs, Nothing)
      pure (Network chan)

-- | Use a reactive network to do IO whenever its side channel fires, whenever
--   its output fires, and whenever it is determined to have stalled.
runNetwork
  :: Network r q
  -> (r -> IO ()) -- ^ Respond to the side-channel.
  -> (q -> IO z)  -- ^ Respond to the final value.
  -> IO z         -- ^ Respond to a lock-up: no more events will fire.
  -> IO z
runNetwork network k f g = catch (runNetwork_ network k f) $
  -- Blocked indefinitely on an mvar means the network is stuck.
  \(_ :: BlockedIndefinitelyOnMVar) -> g

runNetwork_ :: Network r q -> (r -> IO ()) -> (q -> IO z) -> IO z
runNetwork_ network@(Network chan) k f = do
  -- We don't want this readChan to cause the runtime to kill the thread, in
  -- case for instance the network is exhausted and there are no more references
  -- to it (thread blocked indefinitely).
  -- runNetwork catches BlockedIndefinitelyOnMVar and runs some default
  -- IO response.
  (rs, out) <- readChan chan
  _ <- k rs
  case out of
    Just q -> f q
    Nothing -> runNetwork_ network k f
