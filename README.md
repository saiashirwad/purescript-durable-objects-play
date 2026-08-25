# Cloudflare Durable Objects in PureScript.

Live at [durable-mini.texoport.workers.dev](https://durable-mini.texoport.workers.dev/).

```purescript
api :: Worker
api = chat <> counter

chat :: Worker
chat = Worker.make ado
  rooms <- Durable.host roomLive
  in Http.route "/rpc" rooms

counter :: Worker
counter = Worker.make ado
  counters <- Durable.host counterLive
  in Http.route "/rpc" counters
```

```purescript
type RoomApi =
  ( post    :: NewMessage -> Rpc PostError Message
  , history :: Unit       -> Rpc NoError (Array Message)
  , since   :: Int        -> Rpc NoError (Array Message)
  )

room :: Object "Room" RoomApi
room = Durable.object { post: method, history: method, since: method }

-- In the Worker: a real Durable Object stub.
lobby :: Namespace "Room" RoomApi -> Aff (Either (RpcFailure PostError) (Array String))
lobby ns = do
  id <- Durable.newUniqueId ns
  let stub = Durable.get ns id
  Rpc.run do
    _ <- stub.post { author: "ann", text: "hello" }
    _ <- stub.post { author: "bob", text: "hi ann" }
    messages <- Rpc.infallible $ stub.history unit
    pure $ _.text <$> messages

-- In the browser: the same Record RoomApi, over HTTP.
rooms :: Namespace "Room" RoomApi
rooms = Http.connect "/rpc" room

say :: String -> String -> Aff (Either (RpcFailure PostError) Message)
say roomId text = do
  let stub = Durable.get rooms (Durable.idFromString rooms roomId)
  Rpc.run $ stub.post { author: "carol", text }
```

```sh
npm install
npm run dev      # chat at localhost:8787, counter at /counter.html
spago test       # same objects, in-memory simulator
```

Library in `src/Cloudflare/`, chat in `src/Chat/`, pages in `src/Frontend/`.
