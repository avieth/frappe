# frappe: functorial reactive programming

This is a mutation of functional reactive programming in which events yield
values *within some functor*.

To do *functional* reactive programming, we work with `Event t`. There's
one particular combinator with the type `Event (Event t) -> Event t`, which
has the form of monadic join `Monad m => m (m t) -> m t`. It allows the
programmer to use the value of some event to determine what the next event
should be, yielding a composite event which fires when that next event fires.
The motivation for this library is to allow that choice of next event to take
place inside some functor, for instance a reader or state monad.

## Overview

```Haskell
-- Reactive things (Events and Delays) are created inside React a.
type React a t

-- React is MonadIO.
sync :: IO t -> React a t

-- Make a Delay which is fired when an IO completes.
async :: Semigroup r => IO t -> React a (Delay a r f t)

-- Make a Delay and an IO which will fire it.
primEvent :: Semigroup r => React a (Delay a r f t, t -> IO ())

-- A reactive system is an Event which contains f terms and React terms.
-- The rank-2 type ensures that no Event or Delay can escape the Event.
type Reactive r f t = forall a . Event a r (Compose f (React a)) t

-- A reactive system can be run whenever the functor f is in some sense pure:
-- it can be embedded into Identity. State and Reader monads are fine choices.
reactimate
  :: forall a r q f .
     ( Semigroup r, Functor f )
  => Embedding f Identity
  -> Reactive r f q
  -> IO (Network r q)

-- A network needs 3 event handlers: one for the side-channel, one for the
-- final value, and one for the case in which the network stalls (it has not
-- finished, but will never fire again).
runNetwork
  :: Network r q
  -> (Maybe r -> IO ())
  -> (q -> IO z)
  -> IO z
  -> IO z
```
