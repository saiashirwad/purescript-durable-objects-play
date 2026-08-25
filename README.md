# Cloudflare Durable Objects in PureScript.

Live at [durable-mini.texoport.workers.dev](https://durable-mini.texoport.workers.dev/).

```purescript
-- One declaration. The class name is a type.
type RoomApi =
  ( post    :: NewMessage -> Rpc PostError Message
  , history :: Unit       -> Rpc NoError (Array Message)
  , since   :: Int        -> Rpc NoError (Array Message)
  )

room :: Object "Room" RoomApi
room = Durable.object { post: method, history: method, since: method }

-- In the Worker: a real Durable Object stub.
rooms <- Durable.host roomLive
(Durable.get rooms id).post { author, text }

-- In the browser: same Record RoomApi, over HTTP.
rooms = Http.connect "/rpc" room
(Durable.get rooms id).post { author, text }
```

Implementing one:

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
