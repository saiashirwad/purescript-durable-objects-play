module Example.Counter
  ( CounterApi
  , counter
  , counterLive
  ) where

import Prelude

import Cloudflare.Durable (Live, Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (NoError, Rpc, method)
import Cloudflare.Durable.Storage as Storage
import Data.Maybe (fromMaybe)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

type CounterApi =
  ( increment :: Unit -> Rpc NoError Int
  , get :: Unit -> Rpc NoError Int
  )

counter :: Object "Counter" CounterApi
counter = Durable.object
  { increment: method
  , get: method
  }

countKey :: Storage.Key Int
countKey = Storage.key "count"

counterLive :: Live "Counter" CounterApi
counterLive =
  Durable.implement counter ado
    state <- Durable.state
    in
      do
        initial <- Storage.get state countKey
        count <- liftEffect $ Ref.new $ fromMaybe 0 initial
        pure
          { increment: \_ -> do
              next <- liftEffect $ Ref.modify (_ + 1) count
              Storage.put state countKey next
              pure next
          , get: \_ -> liftEffect $ Ref.read count
          }
