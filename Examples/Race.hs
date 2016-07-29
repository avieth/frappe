{-|
Module      : Examples.Race
Description : 
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

Example of very simple concurrency. Two events are raced, so that the value of
the first one to occur (left-biased in case of simultaneity) is given.
This is expressed by the Alternative combinator (<|>).
Different biasing can be expressed via unionEvents.

-}

{-# LANGUAGE ExplicitForAll #-}

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Monad.Embedding
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Void
import Reactive.Frappe

main = do

  let topLevelEvent :: forall a . Event a () (Compose Identity (React a)) Bool
      topLevelEvent = do
        evTrue <- now $ Compose . Identity . async $ threadDelay 500000 >> pure True
        evFalse <- now $ Compose . Identity . async $ threadDelay 1000000 >> pure False
        delayed evTrue <|> delayed evFalse

  let reactiveTerm :: Reactive () Identity Bool
      reactiveTerm = reactive topLevelEvent

  let sideChannel :: Maybe () -> IO ()
      sideChannel _ = pure ()

  network <- reactimate embedIdentity reactiveTerm
  outcome <- runNetwork network
                        -- Fires whenever the side-channel fires (event not
                        -- necessarily done).
                        sideChannel
                        (pure . Just)
                        (pure Nothing)

  -- The network delivers a Bool when it's done. Let's print it.
  print outcome
