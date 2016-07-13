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
  , transEvent
  , emit

  , factorEvent
  , nextSemigroup

  , Delay
  , delayed
  , andThen
  , switchDelay
  , unionDelays
  , applyDelays

  , Accumulator
  , accumulator
  , accumulate
  , accumulateAll

  , Stepper(..)
  , stepper
  , applyStepper
  , (<@>)

  , SyncF

  , Now
  , syncNow
  , asyncNow
  , asyncOSNow
  , primEventNow

  , React
  , sync
  , async
  , asyncOS
  , primEvent

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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Free.Church
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State
import Control.Monad.Cached
import Data.Bifunctor (bimap)
import Data.Semigroup
import Data.List.NonEmpty
import qualified Data.Vault.Lambda as LVault
import Data.MapAlgebra

-- |
-- = Stepper (classical behavior)

newtype Stepper r f t = Stepper {
    getStepper :: (t, Delay r f t)
  }

instance ( Monoid r ) => Functor (Stepper r f) where
  fmap f ~(Stepper (initial, next)) = Stepper (f initial, fmap f next)

instance ( Functor f, Monoid r ) => Applicative (Stepper r f) where
  pure t = Stepper (t, indefiniteDelay)
  ~(Stepper (f, delayf)) <*> ~(Stepper (x, delayx)) = Stepper $
    (y, fmap snd (unionDelays unioner delayf' delayx'))
    where
    unioner ((f, _), _) ((_, x), _) = ((f, x), f x)
    delayf' = fmap (\f -> ((f, x), f x)) delayf
    delayx' = fmap (\x -> ((f, x), f x)) delayx
    y = f x

stepper :: ( Monoid r ) => t -> Delay r f t -> Stepper r f t
stepper t delay = Stepper (t, delay)

applyStepper
  :: forall r f s t .
     ( Monoid r, Functor f )
  => Stepper r f (s -> t)
  -> Delay r f s
  -> Delay r f t
applyStepper ~(Stepper (f, step)) delay =
  mergeDelays merger step delay
  where
  merger
    :: Either (s -> t) (Delay r f (s -> t))
    -> Either s (Delay r f s)
    -> Either t (Delay r f t)
  merger cf cx = case (cf, cx) of
    (Left f', Left x) -> Left (f' x)
    (Left f', Right x) -> Right (applyStepper (stepper f' step) x)
    (Right _, Left x) -> Left (f x)
    (Right f', Right x) -> Right (applyStepper (stepper f f') x)

infixl 4 <@>
(<@>) :: ( Monoid r, Functor f ) => Stepper r f (s -> t) -> Delay r f s -> Delay r f t
(<@>) = applyStepper

-- | Some @f@ with IO mixed in. @SyncF f@ is a functor whenever @f@ is a
--   functor.
--   IO is there for the benefit of Cached. It means that SyncF f is MonadIO.
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

-- Stores functions from pulses (primitives events) to some value.
-- Running an event (primEvent callback) will check the pulses of the top-level
-- event using its own domain key. If it's present, then it will recover the
-- f computation to run to produce a value and the next event.
type Pulses p r f t = MapAlgebra LVault.LambdaVault p (Event r f t)

newtype Delay r f t = Delay {
    getDelay :: forall p . Pulses p r f t
  }

instance ( Monoid r ) => Functor (Delay r f) where
  fmap = mapDelay

delayed :: ( Monoid r ) => Delay r f t -> Event r f t
delayed = Event . cached . pure . Right

-- | TODO explain this.
newtype Event (r :: *) (f :: * -> *) (t :: *) = Event {
    unEvent :: Cached r (SyncF f) (Either t (Delay r f t))
  }

instance ( Monoid r ) => Functor (Event r f) where
  fmap = mapEvent

instance ( Monoid r, Functor f ) => Applicative (Event r f) where
  pure = pureEvent
  -- NB the applicative and monad instances for Event r f are consistent; we
  -- do *not* use (<*>) = applyEvents as that function introduces
  -- concurrency.
  (<*>) = ap

instance ( Monoid r, Functor f ) => Monad (Event r f) where
  return = pure
  ev >>= k = switchEvent (mapEvent k ev)

instance ( Monoid r, Functor f ) => Alternative (Event r f) where
  empty = never
  left <|> right = unionEvents const left right

instance ( Monoid r ) => MonadTrans (Event r) where
  lift = Event . cached . lift . liftF . fmap Left

-- |
-- = Composing events monadically
--
--

-- | Like lift but better, because it doesn't have the Monad f constraint.
now :: ( Monoid r, Functor f ) => f t -> Event r f t
now term = immediate (lift (liftSyncF term))

immediate
  :: forall r f t .
     ( Monoid r )
  => WriterT r (SyncF f) t
  -> Event r f t
immediate term =
  let term' :: WriterT r (SyncF f) (Either t (Delay r f t))
      term' = fmap Left term
  in  Event (cached term')

never :: ( Monoid r ) => Event r f t
never = Event . cached $ pure (Right indefiniteDelay)

indefiniteDelay :: Delay r f t
indefiniteDelay = Delay (Literal LVault.empty)

repeatIndefinitely :: ( Monoid r, Functor f ) => Event r f t -> Event r f x
repeatIndefinitely event =
  let event' = switchEvent (mapEvent (const event') event)
  in  event'

emit :: ( Monoid r ) => r -> Event r f ()
emit r = immediate (tell r)

andThen
  :: ( Monoid r, Functor f )
  => Delay r f s
  -> (s -> Event r f t)
  -> Delay r f t
andThen delay k = Delay $ Fmap (>>= k) (getDelay delay)

pureEvent
  :: forall r f t .
     ( Monoid r )
  => t
  -> Event r f t
pureEvent t = Event . cached $ pure (Left t)

mapEvent
  :: forall r f s t .
     ( Monoid r )
  => (s -> t)
  -> Event r f s
  -> Event r f t
mapEvent f event = Event $ fmap (bimap f (mapDelay f)) (unEvent event)

mapDelay
  :: forall r f s t .
     ( Monoid r )
  => (s -> t)
  -> Delay r f s
  -> Delay r f t
mapDelay f delay = Delay pulses
  where
  pulses :: Pulses p r f t
  pulses = Fmap (mapEvent f) (getDelay delay)

transEvent
  :: forall r f g t .
     ( Functor f, Functor g, Monoid r )
  => (forall t . f t -> g t)
  -> Event r f t
  -> Event r g t
transEvent trans e = undefined
--transEvent trans = embedEvent (naturalEmbedding trans)

{-
embedEvent
  :: forall r f g t .
     ( Functor f, Functor g, Monoid r )
  => Embedding f g
  -> Event r f t
  -> Event r g t
embedEvent embedding = embedEvent_ embedding''
  where
  embedding' :: Embedding (SyncF f) (SyncF g)
  embedding' = syncFEmbedding embedding
  embedding'' :: Embedding (Cached r (SyncF f)) (Cached r (SyncF g))
  embedding'' = cachedEmbedding embedding'

  embedEvent_
    :: Embedding (Cached r (SyncF f)) (Cached r (SyncF g))
    -> Event r f t
    -> Event r g t
  embedEvent_ embedding event = Event $ do
    (choice, embedding') <- runEmbedding embedding (unEvent event)
    case choice of
      Left done -> pure (Left done)
      Right (Delay pulses) -> pure . Right . Delay $
        Fmap (embedEvent_ embedding') pulses
-}

switchEvent
  :: forall r f t .
     ( Monoid r, Functor f )
  => Event r f (Event r f t)
  -> Event r f t
switchEvent event = Event $ do
  choice <- unEvent event
  case choice of
    Left (Event term') -> term'
    Right delay -> pure (Right (switchDelay delay))

switchDelay
  :: forall r f t .
     ( Monoid r, Functor f )
  => Delay r f (Event r f t)
  -> Delay r f t
switchDelay ~(Delay pulses) = Delay pulses'
  where
  pulses' :: Pulses p r f t
  pulses' = fmap switchEvent pulses

infixl 4 >*<
(>*<) :: forall r f s t . ( Monoid r ) => Event r f (s -> t) -> Event r f s -> Event r f t
(>*<) = applyEvents

applyEvents
  :: forall r f s t .
     ( Monoid r )
  => Event r f (s -> t)
  -> Event r f s
  -> Event r f t
applyEvents evf evx = Event $ do
  mf :: Either (s -> t) (Delay r f (s -> t)) <- (unEvent evf)
  mx :: Either s (Delay r f s) <- (unEvent evx)
  pure $ case (mf, mx) of

    (Left f, Left x) -> Left (f x)

    (Left f, Right (Delay pulses)) ->
      Right (Delay (fmap (mapEvent f) pulses))

    (Right (Delay pulses), Left x) ->
      Right (Delay (fmap (mapEvent (flip ($) x)) pulses))

    (Right df, Right dx) -> Right (applyDelays df dx)

applyDelays
  :: forall r f s t .
     ( Monoid r )
  => Delay r f (s -> t)
  -> Delay r f s
  -> Delay r f t
applyDelays ~df@(Delay pulsesf) ~dx@(Delay pulsesx) =
  Delay pulses'
  where

  decomposed :: MapAlgebra LVault.LambdaVault p (PulseDecomp (Event r f (s -> t)) (Event r f s))
  decomposed = decomposePulses pulsesf pulsesx

  pulses' :: Pulses p r f t
  pulses' = recomposePulses handleLeft handleRight handleBoth decomposed

  handleLeft :: Event r f (s -> t) -> Event r f t
  handleLeft event = Event $ do
    choice <- (unEvent event)
    case choice of
      Left f -> pure (Right (Delay (fmap (mapEvent f) pulsesx)))
      Right delay -> pure (Right (applyDelays delay dx))

  handleRight :: Event r f s -> Event r f t
  handleRight event = Event $ do
    choice <- (unEvent event)
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
     ( Monoid r, Functor f )
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
     ( Monoid r )
  => (t -> t -> t)
  -> Delay r f t
  -> Delay r f t
  -> Delay r f t
unionDelays disambiguate = mergeDelays merger
  where
  merger :: Either t (Delay r f t) -> Either t (Delay r f t) -> Either t (Delay r f t)
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
  :: forall x f s t r .
     ( Monoid x )
  => (Either s (Delay x f s) -> Either t (Delay x f t) -> Either r (Delay x f r))
  -> Delay x f s
  -> Delay x f t
  -> Delay x f r
mergeDelays merger delays delayt = Delay pulses

  where

  decomposed :: MapAlgebra LVault.LambdaVault p (PulseDecomp (Event x f s) (Event x f t))
  decomposed = decomposePulses (getDelay delays) (getDelay delayt)

  pulses :: Pulses p x f r
  pulses = recomposePulses handleLeft handleRight handleBoth decomposed

  handleLeft :: Event x f s -> Event x f r
  handleLeft event = Event $ do
    choice <- unEvent event
    pure (merger choice (Right delayt))

  handleRight :: Event x f t -> Event x f r
  handleRight event = Event $ do
    choice <- unEvent event
    pure (merger (Right delays) choice)

  handleBoth :: Event x f s -> Event x f t -> Event x f r
  handleBoth events eventt = Event $ do
    choices <- unEvent events
    choicet <- unEvent eventt
    pure (merger choices choicet)

data PulseDecomp l r where
  PDLeft :: l -> PulseDecomp l r
  PDRight :: r -> PulseDecomp l r
  PDBoth :: l -> r -> PulseDecomp l r

decomposePulses
  :: Pulses p x f l
  -> Pulses p x f r
  -> MapAlgebra LVault.LambdaVault p (PulseDecomp (Event x f l) (Event x f r))
decomposePulses lefts rights = Union unioner lefts' rights'
  where
  lefts' = Fmap PDLeft lefts
  rights' = Fmap PDRight rights
  unioner l r = case (l, r) of
    (PDLeft l', PDRight r') -> PDBoth l' r'
    -- Shame. How to prove this to GHC?
    _ -> error "impossible"

recomposePulses
  :: (Event x f l -> Event q f t)
  -> (Event x f r -> Event q f t)
  -> (Event x f l -> Event x f r -> Event q f t)
  -> MapAlgebra LVault.LambdaVault p (PulseDecomp (Event x f l) (Event x f r))
  -> Pulses p q f t
recomposePulses left right both = Fmap recompose
  where
  recompose term = case term of
    PDLeft l -> left l
    PDRight r -> right r
    PDBoth l r -> both l r

semigroupUnionEvents
  :: ( Functor f, Monoid r, Semigroup t )
  => Event r f t
  -> Event r f t
  -> Event r f t
semigroupUnionEvents = unionEvents (<>)

factorEvent
  :: forall r q f s t .
     ( Monoid q )
  => (r -> Either (Event r f s) s -> Event q f t)
  -> Event r f s
  -> Event q f t
factorEvent k event = Event $ do
  (choice, rs) <- withResidues (unEvent event)
  case choice of
    Left s -> unEvent (k rs (Right s))
    Right (Delay pulses) -> pure (Right (Delay (fmap (k rs . Left) pulses)))

nextSemigroup
  :: ( Functor f, Semigroup r, Monoid q )
  => Event [r] f t
  -> Event q f r
nextSemigroup = factorEvent $ \rs choice -> case nonEmpty rs of
  Just ne -> pure (sconcat ne)
  Nothing -> case choice of
    Left event' -> nextSemigroup event'
    Right _ -> never

newtype Accumulator r f t = Accumulator {
    getAccumulator :: Event r f (t, Maybe (Accumulator r f t))
  }

accumulate
  :: forall r f s t .
     ( Functor f, Monoid r )
  => (s -> Maybe (Accumulator r f s) -> Event r f t)
  -> Accumulator r f s
  -> Event r f t
accumulate k acc = do
  (t, next) <- getAccumulator acc
  k t next

-- | Take the final value of an accumulator. It's done when the accumulator
--   input event is done.
accumulateAll
  :: ( Functor f, Monoid r )
  => Accumulator r f t
  -> Event r f t
accumulateAll = accumulate $ \t next -> case next of
  Nothing -> pure t
  Just acc -> accumulateAll acc

-- | Repeatedly apply the side-channel output of some event to an initial
--   value.
accumulator
  :: forall f r t anything .
     ( Functor f, Monoid r )
  => t
  -> Event (Option (Last (t -> t))) f anything
  -> Accumulator r f t
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
      next :: Accumulator r f t
      next = accumulator acc' event'

data NowEnv (r :: *) (f :: * -> *) (t :: *) = NowEnv {
    nowChan :: Chan (r, Maybe t) -- ^ Dump outputs here
  , nowEval :: MVar (Delay r f t, Embedding f IO)
  }

newtype Now (r :: *) (q :: *) (f :: * -> *) (t :: *) = Now {
    runNow :: ReaderT (NowEnv r f q) IO t
  }

deriving instance Functor (Now r q f)
deriving instance Applicative (Now r q f)
deriving instance Monad (Now r q f)

newtype React (t :: *) = React {
    runReact :: forall r q f . ( Monoid r, Functor f ) => Now r q f t
  }

deriving instance Functor React

instance Applicative React where
  pure x = React $ pure x
  React mf <*> React mx = React $ mf <*> mx

instance Monad React where
  return = pure
  React x >>= k = React $ x >>= runReact . k

embedNow :: NowEnv r f q -> Embedding (Now r q f) IO
embedNow nowEnv =
  let embedding = Embedding $ \(Now reader) -> do
        out <- runReaderT reader nowEnv
        pure (out, embedding)
  in  embedding

embedReact :: forall r q f . (Monoid r, Functor f) => Embedding React (Now r q f)
embedReact = Embedding $ \react -> fmap (flip (,) embedReact) (runReact react)

syncNow :: IO t -> Now r q f t
syncNow io = Now $ ReaderT $ \_ -> io

sync :: IO t -> React t
sync io = React $ syncNow io

asyncNow :: ( Monoid r, Monoid x, Functor f ) => IO t -> Now r q f (Delay x g t)
asyncNow io = do
  (ev, cb) <- primEventNow
  _ <- syncNow $ do result <- Async.async (io >>= cb)
                    Async.link result
  pure ev

async :: ( Monoid x ) => IO t -> React (Delay x f t)
async io = React $ asyncNow io

asyncOSNow :: ( Monoid r, Monoid x, Functor f ) => IO t -> Now r q f (Delay x g t)
asyncOSNow io = do
  (ev, cb) <- primEventNow
  syncNow (Async.withAsyncBound (io >>= cb) (const (pure ev)))

asyncOS :: ( Monoid x ) => IO t -> React (Delay x f t)
asyncOS io = React $ asyncOSNow io

-- | A primitive event and a function to fire it.
primEventNow
  :: forall r q x f g t .
     ( Monoid r, Monoid x, Functor f )
  => Now r q f (Delay x g t, t -> IO ())
primEventNow = Now $ ReaderT $ \nowEnv -> do
  domainKey :: LVault.DomainKey t <- LVault.newDomainKey
  let pulse :: t -> Event x g t
      pulse = \t -> Event (cached (pure (Left t)))
      thisPulses :: Pulses p x g t
      thisPulses = Literal (LVault.insert domainKey pulse LVault.empty)
      delay :: Delay x g t
      delay = Delay thisPulses
  -- When the event fires, we grab the top-level event and check whether
  -- this event determines a computation for it. If it does, run it to
  -- come up with output and the next event.
  let cb = \t -> do stuff@(Delay pulses, embedding)
                      <- takeMVar (nowEval nowEnv)
                    (pulsesMap, _) <- runMapAlgebra LVault.empty LVault.union fmap pulses
                    pulsesMap `seq` (pure ())
                    case LVault.lookup domainKey pulsesMap of
                      Nothing -> do
                        --trace ("Squelching unobserved event") (pure ())
                        putMVar (nowEval nowEnv) stuff
                      Just (computation :: t -> Event r f q) -> do
                        let Event cached = computation t
                            syncf :: SyncF f (Either q (Delay r f q), r)
                            syncf = runWriterT (runCached cached)
                            statet :: StateT (Embedding f IO) IO (Either q (Delay r f q), r)
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
  pure (delay, cb)

primEvent :: ( Monoid x ) => React (Delay x f t, t -> IO ())
primEvent = React primEventNow

newtype Network r q = Network (Chan (r, Maybe q))

-- | Use an embedding of f into React to create an event network.
reactimate
  :: forall r q f .
     ( Monoid r, Monad f )
  => Embedding f React
  -> f (Event r f q)
  -> IO (Network r q)
reactimate fembedding fterm = do
  chan <- newChan
  mvar <- newEmptyMVar
  let nowEnv = NowEnv chan mvar
  let nowembedding = embedNow nowEnv
  let embedding :: Embedding f IO
      embedding = nowembedding `composeEmbedding` embedReact
                               `composeEmbedding` fembedding
  (Event cached, embedding') <- runEmbedding embedding fterm
  let syncf = runWriterT (runCached cached)
      statet :: StateT (Embedding f IO) IO (Either q (Delay r f q), r)
      statet = embedSyncF syncf
  ((choice, rs), embedding'') <- runStateT statet embedding'
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
