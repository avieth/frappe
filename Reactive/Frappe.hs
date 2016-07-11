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
  , applyEvents
  , (>*<)
  , unionEvents
  , semigroupUnionEvents
  , repeatIndefinitely
  , commuteEvent
  , transEvent

  , Stepper
  , stepper
  , applyStepper
  , (<@>)

  , Sampler
  , sampler
  , sample
  , (<!>)

  , SyncF

  , Now
  , sync
  , async
  , asyncOS
  , primEvent

  , Network
  , reactimate
  , runNetwork

  ) where

import Control.Applicative
import Control.Monad (ap)
import Control.Exception
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Monad.Embedding
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Free.Church
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State
import Control.Monad.Cached
import qualified Data.Vault.Lambda as LVault
import Data.Bifunctor (bimap)
import Data.Semigroup

-- |
-- = Sampler
--
-- A sampler is an immediate event which can be constructed from a term which
-- includes IO. This allows us to express something like a pull-based behavior:
-- a reactive-banana behavior which is computed when it's sampled, rather than
-- only updated when an event fires. The IO will be run at most once when an
-- event fires. A great use case is for the DOM: sampling an element's
-- dimensions as needed, rather than fumbling to try and keep them up to date
-- in terms of steppers (not sure how to do that in a sane way).

-- | An f with IO computation. The Event in here is always an immediate.
--
--   Can only be constructed by @sampler@, and only used by @sample@.
newtype Sampler r f t = Sampler {
    unSampler :: Cached r (SyncF f) t
  }

instance Functor (Sampler r f) where
  fmap f = Sampler . fmap f . unSampler

-- | Construct a sampler from some IO computation.
sampler :: IO t -> Sampler r f t
sampler io = Sampler (cached (lift (lift io)))

-- | Sample a Sampler when some event fires.
sample :: Sampler r f (s -> t) -> Event r f s -> Event r f t
sample (Sampler cached) = mapEventCached (\t -> cached <*> pure t)

infixl 4 <!>
(<!>) :: Sampler r f (s -> t) -> Event r f s -> Event r f t
(<!>) = sample

-- |
-- = Stepper (classical behaviors)
--
-- How can we simulate the classical behavior?
-- Imagine an event where the functor has a ReaderT r over it. Whenever it runs
-- it has access to some value, but it cannot change that value.
-- If we could somehow control the embedding via some other event...
--

-- | A single-step function.
newtype Stepper r f t = Stepper {
    getStepper :: (Cached r (SyncF f) t, Event r f t)
  }

instance Functor (Stepper r f) where
  fmap f (Stepper (initial, next)) = Stepper (fmap f initial, fmap f next)

stepper :: SyncF f t -> Event r f t -> Stepper r f t
stepper syncf event = Stepper (cachedTerm, event)
  where
  cachedTerm = cached (lift syncf)

-- If the stepper's event fires strictly before the second event, use the
-- resulting function when the second event fires; otherwise, use the initial
-- value.
applyStepper
  :: forall r f s t .
     ( Functor f )
  => Stepper r f (s -> t)
  -> Event r f s
  -> Event r f t
applyStepper ~(Stepper (initial, step)) event =
  -- We race two events:
  --   - @sooner@ fires when @event@ fires, giving an immediate event with the
  --     initial function applies to its value.
  --   - @later@ fires when @step@ fires, giving @event@ with the now known
  --     function applied.
  -- In case of coincidence, we take sooner (the original value of the
  -- stepper).
  switchEvent (unionEvents const sooner later)
  where
  sooner :: Event r f (Event r f t)
  sooner = fmap pure <$> Event (fmap Left initial) >*< event
  later :: Event r f (Event r f t)
  later = fmap (\f -> fmap f event) step

infixl 4 <@>
(<@>) :: Functor f => Stepper r f (s -> t) -> Event r f s -> Event r f t
(<@>) = applyStepper

-- TBD is this single-step presentation sufficient to do all that lovely
-- classical reactive programming?

-- Stores functions from pulses (primitives events) to some value.
-- Running an event (primEvent callback) will check the pulses of the top-level
-- event using its own domain key. If it's present, then it will recover the
-- f computation to run to produce a value and the next event.
type Pulses r f t = LVault.LambdaVault (Event r f t)

-- Motivation for the first parameter r... when we bindEvent, we're saying
-- to continue with some synchronous f-computation when the event occurs.
-- If we have Event r f Void then we know that the event will never come to
-- completion; there will always be another delayed part. Since r is there,
-- we still have a way to get data from it. We can't add any further "lateral"
-- f-computation, but we should be able to derive other events from this
-- r side-channel.
--
-- How to deal with this assymmetry, though? Consider event unioning for
-- instance. We can union on the third parameter, that's straightforward. But
-- surely we can also union on the first parameter.
--
-- Ok how's this for justification: the first parameter is cross-thread
-- communication and the third parameter is the final output of the thread.
-- A thread computes some value, but also offers a way to communicate with
-- other threads. Input is expressed by deriving an event from some other
-- thread's first parameter, and output is symmetric (some other thread is
-- derived from this event's first parameter).
--
--   -- If the event never gives an r, then the resulting event is never.
--   communicate
--     :: (NonEmpty r -> Maybe (Event r f t) -> Event s f u)
--     -> Event r f t
--     -> Event s f u

-- | Some @f@ with IO mixed in. @SyncF f@ is a functor whenever @f@ is a
--   functor.
type SyncF f = FT f IO

liftSyncIO :: IO t -> SyncF f t
liftSyncIO = lift

liftSyncF :: ( Functor f ) => f t -> SyncF f t
liftSyncF = liftF

liftWriterSync
  :: ( Functor f )
  => WriterT [r] f t
  -> WriterT [r] (SyncF f) t
liftWriterSync = WriterT . liftSyncF . runWriterT

-- | A SyncF can be injected into IO by using an Embedding.
embedSyncF
  :: forall f t .
     ( Functor f )
  => SyncF f t
  -> StateT (Embedding f IO) IO t
embedSyncF = iterTM embed
  where
  embed :: forall t . f (StateT (Embedding f IO) IO t) -> StateT (Embedding f IO) IO t
  embed term = do
    embedding <- get
    (t, embedding') <- lift (runEmbedding embedding term)
    _ <- put embedding'
    t

-- Is this the good presentation? It allows us to express
--
--   1. Immediate events, giving a good candidate for Applicative's pure.
--   2. A side channel which can also be immediate.
--
--
newtype Event (r :: *) (f :: * -> *) (t :: *) = Event {
    unEvent :: Cached r (SyncF f) (Either t (Delay r f t))
  }

data Delay r f t where
  Delay :: Pulses r f t -> Delay r f t

instance Functor (Event r f) where
  fmap = mapEvent

instance Functor f => Applicative (Event r f) where
  pure = pureEvent
  -- NB the applicative and monad instances for Event r f are consistent; we
  -- do *not* use (<*>) = applyEvents as that function introduces
  -- concurrency.
  (<*>) = ap

instance Functor f => Monad (Event r f) where
  return = pure
  (>>=) = andThen

instance (Functor f) => Alternative (Event r f) where
  empty = never
  left <|> right = unionEvents const left right

-- |
-- = Composing events monadically
--
--   do t <- someExistingEvent
--      -- The computation in now should be cached as one unit.
--      -- Breaking it up should be fine, i.e. now should be observably
--      -- homomorphic:
--      --   now (f `join` g) = now f `join` now g
--      -- although operationally it's different as there will be more cache
--      -- entries in the latter.
--      r <- now $ do x <- lift someSpecialAppComputation
--                    _ <- tell [x]
--                    pure x
--      fmap ((,) r) someOtherExistingEvent
--
--  The issue is that, in order to make that cached thing, we need some IO.
--  Perhaps instead we should use stable names for the cache? Suppose we did.
--  We do. It's great.
--
--
--

now :: ( Functor f ) => WriterT [r] f t -> Event r f t
now writer = immediate term
  where
  term = WriterT (liftSyncF (runWriterT writer))

immediate
  :: forall r f t .
     ( )
  => WriterT [r] (SyncF f) t
  -> Event r f t
immediate term =
  let term' :: WriterT [r] (SyncF f) (Either t (Delay r f t))
      term' = fmap Left term
  in  Event (cached term')

delayed :: Delay r f t -> Event r f t
delayed = Event . pure . Right

fromDelay
  :: forall r f t .
     Delay r f t
  -> Event r f t
fromDelay delay = Event (pure (Right delay))

never :: Event r f t
never = Event $ pure (Right indefiniteDelay)

indefiniteDelay :: Delay r f t
indefiniteDelay = Delay LVault.empty

repeatIndefinitely :: ( Functor f ) => Event r f t -> Event r f x
repeatIndefinitely event =
  let event' = switchEvent (mapEvent (const event') event)
  in  event'

-- | Monadic bind for events.
andThen
  :: ( Functor f )
  => Event r f s
  -> (s -> Event r f t)
  -> Event r f t
andThen ev next = switchEvent (mapEvent next ev)

pureEvent
  :: forall r f t .
     ( )
  => t
  -> Event r f t
pureEvent t = Event $ pure (Left t)

commuteEvent
  :: forall r g x y f t .
     ( )
  => Event r g (Now x y f t)
  -> Now x y f (Event r g t)
commuteEvent event = Now $ do
  nowEnv <- ask
  -- clear Now using nowEnv and inject the IO into the cached computation.
  let k :: Now x y f t -> WriterT [r] (SyncF g) t
      k now = lift (liftSyncIO (runReaderT (runNow now) nowEnv))
  -- mapEventF will cache the k-computation.
  pure (mapEventF k event)

mapEvent :: forall r f s t . (s -> t) -> Event r f s -> Event r f t
mapEvent f event = Event $ fmap (bimap f (mapDelay f)) (unEvent event)

mapDelay :: forall r f s t . (s -> t) -> Delay r f s -> Delay r f t
mapDelay f (Delay pulses) = Delay pulses'
  where
  pulses' :: Pulses r f t
  pulses' = fmap (mapEvent f) pulses

-- | Bind a cached computation through an event.
mapEventCached
  :: forall r f s t .
     ( )
  => (s -> Cached r (SyncF f) t)
  -> Event r f s
  -> Event r f t
mapEventCached k event = Event $ do
  choice <- unEvent event
  case choice of
    Left t -> fmap Left (k t)
    Right delay -> pure (Right (mapDelayCached k delay))

mapDelayCached
  :: forall r f s t .
     ( )
  => (s -> Cached r (SyncF f) t)
  -> Delay r f s
  -> Delay r f t
mapDelayCached k (Delay pulses) = Delay pulses'
  where
  pulses' :: Pulses r f t
  pulses' = fmap (mapEventCached k) pulses

-- | Cache a computation and bind it through an event.
mapEventF
  :: forall r f s t .
     ( )
  => (s -> WriterT [r] (SyncF f) t)
  -> Event r f s
  -> Event r f t
mapEventF k event =
  -- We use the same key for every term which comes out of the k.
  -- I suspect this is OK but I am not sure.
  -- Is it true that the Kleisli arrow @k@ will be run at most once for every
  -- event trigger? Well, sure: whenever the event known here as @event@ is
  -- run.
  mapEventCached (cached . k) event

transEvent
  :: forall r f g t .
     ( Functor g )
  => (forall t . f t -> g t)
  -> Event r f t
  -> Event r g t
transEvent trans event = Event term'
  where
  term' :: Cached r (SyncF g) (Either t (Delay r g t))
  term' = (fmap . fmap) (transDelay trans) (transCached (transFT trans) (unEvent event))

transDelay
  :: forall r f g t .
     ( Functor g )
  => (forall t . f t -> g t)
  -> Delay r f t
  -> Delay r g t
transDelay trans (Delay pulses) = Delay pulses'
  where
  pulses' :: Pulses r g t
  pulses' = fmap (transEvent trans) pulses

switchEvent
  :: forall r f t .
     ( Functor f )
  => Event r f (Event r f t)
  -> Event r f t
switchEvent event = Event $ do
  choice <- unEvent event
  case choice of
    Left (Event term') -> term'
    Right delay -> pure (Right (switchDelay delay))

switchDelay
  :: forall r f t .
     ( Functor f )
  => Delay r f (Event r f t)
  -> Delay r f t
switchDelay (Delay pulses) = Delay pulses'
  where
  pulses' :: Pulses r f t
  pulses' = fmap switchEvent pulses

infixl 4 >*<
(>*<) :: forall r f s t . Event r f (s -> t) -> Event r f s -> Event r f t
(>*<) = applyEvents

applyEvents
  :: forall r f s t .
     Event r f (s -> t)
  -> Event r f s
  -> Event r f t
applyEvents evf evx = Event $ do
  mf :: Either (s -> t) (Delay r f (s -> t)) <- unEvent evf
  mx :: Either s (Delay r f s) <- unEvent evx
  pure $ case (mf, mx) of

    (Left f, Left x) -> Left (f x)

    (Left f, Right (Delay pulses)) ->
      Right (Delay (fmap (mapEvent f) pulses))

    (Right (Delay pulses), Left x) ->
      Right (Delay (fmap (mapEvent (flip ($) x)) pulses))

    (Right df, Right dx) -> Right (applyDelays df dx)

applyDelays
  :: forall r f s t .
     ( )
  => Delay r f (s -> t)
  -> Delay r f s
  -> Delay r f t
applyDelays df@(Delay pulsesf) dx@(Delay pulsesx) =
  Delay pulses'
  where

  decomposed :: LVault.LambdaVault (PulseDecomp (Event r f (s -> t)) (Event r f s))
  decomposed = decomposePulses pulsesf pulsesx

  pulses' :: Pulses r f t
  pulses' = recomposePulses handleLeft handleRight handleBoth decomposed

  handleLeft :: Event r f (s -> t) -> Event r f t
  handleLeft event = Event $ do
    choice <- unEvent event
    case choice of
      Left f -> pure (Right (Delay (fmap (mapEvent f) pulsesx)))
      Right delay -> pure (Right (applyDelays delay dx))

  handleRight :: Event r f s -> Event r f t
  handleRight event = Event $ do
    choice <- unEvent event
    case choice of
      Left x -> pure (Right (Delay (fmap (mapEvent (flip ($) x)) pulsesf)))
      Right delay -> pure (Right (applyDelays df delay))

  handleBoth :: Event r f (s -> t) -> Event r f s -> Event r f t
  handleBoth = applyEvents


-- Unions the side-channel (first parameter, the r's) and races the final
-- value (third parameter). If they both come simultaneously, the
-- disambiguator goes to work. No disambiguator needed for the side-channel
-- of course, as they're gathered in a list.
--
unionEvents
  :: forall r f t .
     ( Functor f )
  => (t -> t -> t)
  -> Event r f t
  -> Event r f t
  -> Event r f t
unionEvents disambiguate evl evr = Event $ do
  -- We don't cache this part of the computation. That's fine, as the
  -- computations inside evl, evr will be appropriately cached, and what we
  -- do here introduces no extra side-effects.
  l :: Either t (Delay r f t) <- unEvent evl
  r :: Either t (Delay r f t) <- unEvent evr
  pure $ case (l, r) of
    (Left tl, Left tr) -> Left (disambiguate tl tr)
    (Left tl, Right _) -> Left tl
    (Right _, Left tr) -> Left tr
    (Right dl, Right dr) -> Right (unionDelays disambiguate dl dr)

unionDelays
  :: forall r f t .
     ( Functor f )
  => (t -> t -> t)
  -> Delay r f t
  -> Delay r f t
  -> Delay r f t
unionDelays disambiguate dl@(Delay pulsesl) dr@(Delay pulsesr) =
  Delay pulses'
  where

  decomposed :: LVault.LambdaVault (PulseDecomp (Event r f t) (Event r f t))
  decomposed = decomposePulses pulsesl pulsesr

  pulses' :: Pulses r f t
  pulses' = recomposePulses handleLeft handleRight handleBoth decomposed

  handleLeft :: Event r f t -> Event r f t
  handleLeft event = Event $ do
    choice <- unEvent event
    case choice of
      Left l -> pure (Left l)
      Right delay -> pure (Right (unionDelays disambiguate delay dr))

  handleRight :: Event r f t -> Event r f t
  handleRight event = Event $ do
    choice <- unEvent event
    case choice of
      Left r -> pure (Left r)
      Right delay -> pure (Right (unionDelays disambiguate dl delay))

  handleBoth :: Event r f t -> Event r f t -> Event r f t
  handleBoth = unionEvents disambiguate

data PulseDecomp l r where
  PDLeft :: l -> PulseDecomp l r
  PDRight :: r -> PulseDecomp l r
  PDBoth :: l -> r -> PulseDecomp l r

decomposePulses
  :: Pulses x f l
  -> Pulses x f r
  -> LVault.LambdaVault (PulseDecomp (Event x f l) (Event x f r))
decomposePulses lefts rights = LVault.union unioner lefts' rights'
  where
  lefts' = fmap PDLeft lefts
  rights' = fmap PDRight rights
  unioner l r = case (l, r) of
    (PDLeft l', PDRight r') -> PDBoth l' r'
    -- Shame. How to prove this to GHC?
    _ -> error "impossible"

recomposePulses
  :: (Event x f l -> Event q f t)
  -> (Event x f r -> Event q f t)
  -> (Event x f l -> Event x f r -> Event q f t)
  -> LVault.LambdaVault (PulseDecomp (Event x f l) (Event x f r))
  -> Pulses q f t
recomposePulses left right both = fmap recompose
  where
  recompose term = case term of
    PDLeft l -> left l
    PDRight r -> right r
    PDBoth l r -> both l r

semigroupUnionEvents
  :: ( Functor f, Semigroup t )
  => Event r f t
  -> Event r f t
  -> Event r f t
semigroupUnionEvents = unionEvents (<>)

-- This may prove *very* useful: get an event which fires whenever the side
-- channel *or* the final value fires. If the side channel fires, give the
-- continuation.
--
-- NB it's always immediate! Is that a problem?
factorEvent
  :: forall r q f t .
     ( )
  => Event r f t
  -> Event q f (Either (Delay r f t, [r]) (t, [r]))
factorEvent event = event'
  where
  event' :: Event q f (Either (Delay r f t, [r]) (t, [r]))
  event' = Event $ do
    (choice, rs) <- withResidues (unEvent event)
    case choice of
      Left t -> pure (Left (Right (t, rs)))
      Right (delay :: Delay r f t) -> pure (Left (Left (delay, rs)))

data NowEnv r f t = NowEnv {
    nowChan :: Chan ([r], Maybe t)
  , nowEval :: MVar (Delay r f t, Embedding f IO)
  }

newtype Now (r :: *) (q :: *) (f :: * -> *) (t :: *) = Now {
    runNow :: ReaderT (NowEnv r f q) IO t
  }

deriving instance Functor (Now r q f)
deriving instance Applicative (Now r q f)
deriving instance Monad (Now r q f)

sync :: IO t -> Now r q f t
sync io = Now $ ReaderT $ \_ -> io

async :: Functor f => IO t -> Now r q f (Event x f t)
async io = do
  (ev, cb) <- primEvent
  _ <- sync $ do result <- Async.async (io >>= cb)
                 Async.link result
  pure ev

asyncOS :: Functor f => IO t -> Now r q f (Event x f t)
asyncOS io = do
  (ev, cb) <- primEvent
  sync (Async.withAsyncBound (io >>= cb) (const (pure ev)))

-- | A primitive event and a function to fire it.
primEvent :: forall r q x f t . Functor f => Now r q f (Event x f t, t -> IO ())
primEvent = Now $ ReaderT $ \nowEnv -> do
  domainKey :: LVault.DomainKey t <- LVault.newDomainKey
  let pulse :: t -> Event x f t
      pulse = \t -> Event (cached (pure (Left t)))
      thisPulses :: Pulses x f t
      thisPulses = LVault.insert domainKey pulse LVault.empty
      event :: Event x f t
      event = Event (cached (pure (Right (Delay thisPulses))))
  -- When the event fires, we grab the top-level event and check whether
  -- this event determines a computation for it. If it does, we recover a
  -- function  t -> f ((), Event f ())
  let cb = \t -> do stuff@(Delay pulses, embedding)
                      <- takeMVar (nowEval nowEnv)
                    case LVault.lookup domainKey pulses of
                      Nothing -> do
                        --_ <- putStrLn "Event unobserved; squelching"
                        putMVar (nowEval nowEnv) stuff
                      Just (computation :: t -> Event r f q) -> do
                        let Event cached = computation t
                            syncf :: SyncF f (Either q (Delay r f q), [r])
                            syncf = runWriterT (runCached cached)
                            statet :: StateT (Embedding f IO) IO (Either q (Delay r f q), [r])
                            statet = embedSyncF syncf
                        ((choice' :: Either q (Delay r f q), rs), embedding')
                          <- runStateT statet embedding
                        _ <- case choice' of
                          Left done -> do
                            _ <- writeChan (nowChan nowEnv) (rs, Just done)
                            pure ()
                          Right delay -> do
                            _ <- putMVar (nowEval nowEnv) (delay, embedding')
                            _ <- writeChan (nowChan nowEnv) (rs, Nothing)
                            pure ()
                        pure ()
  pure (event, cb)


newtype Network r q = Network (Chan ([r], Maybe q))

reactimate
  :: forall r q f .
     ( Monad f )
  => Embedding f IO
  -> Now r q f (Event r f q)
  -> IO (Network r q)
reactimate embedding (Now runIt) = do
  chan <- newChan
  mvar <- newEmptyMVar
  let nowEnv = NowEnv chan mvar
  Event cached <- runReaderT runIt nowEnv
  let syncf = runWriterT (runCached cached)
      statet = embedSyncF syncf
  ((choice, rs), embedding') <- runStateT statet embedding
  case choice of
    Left done -> do
      writeChan chan (rs, Just done)
    Right delay -> do
      _ <- putMVar mvar (delay, embedding')
      writeChan chan (rs, Nothing)
  pure (Network chan)

runNetwork
  :: Network r q
  -> ([r] -> IO ()) -- ^ Respond to the side-channel.
  -> (q -> IO z)    -- ^ Respond to the final value.
  -> IO z           -- ^ Respond to a lock-up: no more events will fire.
  -> IO z
runNetwork network k f g = catch (runNetwork_ network k f) $
  -- Blocked indefinitely on an mvar means the network is stuck.
  \(e :: BlockedIndefinitelyOnMVar) -> g

runNetwork_ :: Network r q -> ([r] -> IO ()) -> (q -> IO z) -> IO z
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
