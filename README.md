# Cloudflare Durable Objects in PureScript.

Live at [durable-mini.texoport.workers.dev](https://durable-mini.texoport.workers.dev/).

```purescript
type RoomApi =
  ( post    :: NewMessage -> Rpc PostError Message
  , history :: Unit       -> Rpc NoError (Array Message)
  , since   :: Int        -> Rpc NoError (Array Message)
  )

room :: Object "Room" RoomApi
room = Durable.object { post: method, history: method, since: method }

-- In the Worker: host the object, get a real Durable Object stub, call it.
api :: Worker
api = Worker.make ado
  rooms <- Durable.host roomLive
  in
    { fetch: \_ -> do
        id <- Durable.newUniqueId rooms
        let lobby = Durable.get rooms id
        result <- Rpc.run do
          _ <- lobby.post { author: "ann", text: "hello" }
          _ <- lobby.post { author: "bob", text: "hi ann" }
          Rpc.infallible $ lobby.history unit
        pure case result of
          Right messages -> Worker.text 200 $ show (_.text <$> messages)
          Left failure -> Worker.text 500 $ show failure
    }

-- In the browser: the same Record RoomApi, over HTTP.
rooms :: Namespace "Room" RoomApi
rooms = Http.connect "/rpc" room

say :: String -> String -> Aff (Either (RpcFailure PostError) Message)
say roomId text = do
  let lobby = Durable.get rooms (Durable.idFromString rooms roomId)
  Rpc.run $ lobby.post { author: "carol", text }
```

```sh
npm install
npm run dev      # chat at localhost:8787, counter at /counter.html
spago test       # same objects, in-memory simulator
```

Library in `src/Cloudflare/`, chat in `src/Chat/`, pages in `src/Frontend/`.
