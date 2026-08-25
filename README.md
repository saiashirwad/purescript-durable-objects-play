# durable-mini

Cloudflare Durable Objects in PureScript. Declare an object once; the
implementation and every client stub share the same record type.

```purescript
type CounterApi =
  ( increment :: Unit -> Rpc NoError Int
  , get :: Unit -> Rpc NoError Int
  )

counter :: Object "Counter" CounterApi
counter = Durable.object { increment: method, get: method }

countKey :: Storage.Key Int
countKey = Storage.key "count"

counterLive :: Live "Counter" CounterApi
counterLive =
  Durable.implement counter ado
    state <- Durable.state
    in do
      initial <- Storage.get state countKey
      count <- liftEffect $ Ref.new $ fromMaybe 0 initial
      pure
        { increment: \_ -> do
            next <- liftEffect $ Ref.modify (_ + 1) count
            Storage.put state countKey next
            pure next
        , get: \_ -> liftEffect $ Ref.read count
        }

api :: Worker
api = Worker.make ado
  counters <- Durable.host counterLive
  in
    { fetch: \_ -> do
        result <- Rpc.run $ (Durable.getByName counters "user-123").increment unit
        pure $ Worker.text 200 $ show result
    }
```

```sh
npm install
npm run dev      # chat at localhost:8787, counter at /counter.html
spago test       # same objects, in-memory simulator
```

Library in `src/Cloudflare/`, chat in `src/Chat/`, pages in `src/Frontend/`.
