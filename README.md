# Cloudflare Durable Objects in PureScript.

Live at [durable-mini.texoport.workers.dev](https://durable-mini.texoport.workers.dev/).

```purescript
api :: Worker
api = login <> Worker.protect access (chat <> counter)

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
  , react   :: { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
  , history :: Unit       -> Rpc NoError (Array Message)
  , members :: Unit       -> Rpc NoError (Array String)
  , typing  :: String     -> Rpc NoError Unit
  )

-- Pushed to every open WebSocket, typed as a row too.
type RoomEvents = ( message :: Message, updated :: Message, joined :: String, left :: String, typing :: String )

room :: Object "Room" RoomApi RoomEvents
room =
  Durable.object { post: method, react: method, history: method, members: method, typing: method }
    `Durable.emitting` { message: event, updated: event, joined: event, left: event, typing: event }

-- In the Worker: a real Durable Object stub.
lobby :: Namespace "Room" RoomApi RoomEvents -> Aff (Either (RpcFailure PostError) (Array String))
lobby ns = do
  id <- Durable.newUniqueId ns
  let stub = Durable.get ns id
  Rpc.run do
    _ <- stub.post { author: "ann", text: "hello", images: [], replyTo: Nothing }
    messages <- Rpc.infallible $ stub.history unit
    pure $ _.text <$> messages

-- In the browser: the same Record RoomApi, over HTTP.
rooms :: Namespace "Room" RoomApi RoomEvents
rooms = Http.connect "/rpc" room

-- ...and its events over a WebSocket that hibernates with the object.
feed :: RoomId -> Emitter (Signal (Variant RoomEvents))
feed id = makeEmitter $ Durable.listen rooms id "carol"
```

Storage, alarms and SQL all go through one `State`, so the simulator and the
Worker answer the same calls:

```purescript
-- Typed key-value: a Prefix is a family of Keys sharing a codec.
fired :: Storage.Prefix String
fired = Storage.prefix "fired:"

-- SQL: a Statement is a Profunctor. Params encode contravariantly (Divisible),
-- rows decode applicatively.
insert :: Statement { account :: String, amount :: Int } Unit
insert = lcmap (\e -> e.account /\ e.amount) $ Sql.statement
  "INSERT INTO entries (account, amount) VALUES (?, ?)"
  (Sql.param CA.string `divided` Sql.param CA.int)
  (pure unit)

byAccount :: Statement String { id :: Int, amount :: Int }
byAccount = Sql.statement
  "SELECT id, amount FROM entries WHERE account = ? ORDER BY id"
  Sql.paramOf
  ({ id: _, amount: _ } <$> Sql.columnOf "id" <*> Sql.columnOf "amount")

-- Alarms: `alarm :: Runtime Unit` is a Monoid; `mempty` means no alarm.
reminderLive = Durable.implementWith reminder ado
  state <- Durable.state
  in pure $ (Durable.handlers { remind: \{ after, note } -> ... })
    { alarm = Storage.get state noteKey >>= traverse_ \note ->
        Storage.put state (fired `Storage.at` "0") note }

-- Inside the object: sockets are a Contravariant channel. `cmap` narrows it
-- to one event; `connect`/`disconnect` hooks are monoids like `alarm`.
roomLive = Durable.implementWith room $ map handlersFor <$> open
  where
  open = ado
    state <- Durable.state
    sockets <- Durable.sockets room
    in do ...
  handlersFor r = (Durable.handlers { post: post r, ... })
    { connect = Sockets.broadcast (cmap (inj (Proxy :: _ "joined")) sockets) <<< _.tag }

-- A container is declared where it is used; wrangler config follows.
echoLive = Durable.implementWith echo ado
  state <- Durable.state
  box <- Durable.container (Container.image "./containers/echo/Dockerfile")
  in pure $ (Durable.handlers { ... })
    { fetch = \request -> do
        Container.ensure box 8080 $ Container.env [ "GREETING" /\ "hi" ] <> Container.noInternet
        Container.renew state box (Minutes 5.0)
        Container.request box 8080 request
    , alarm = Container.expire state box }

-- Agents: a Def names no model; mount attaches one and some tools.
assistant :: Agent Runtime String String
assistant = mount (Model.hoist liftAff model) [ whoIsHere ] $ text "You are terse."

-- In tests, time is a Clock you advance; due alarms fire during `advance`.
clock <- Simulator.clock
reminders <- Simulator.simulateOn clock reminderLive
Simulator.advance clock (Milliseconds 1000.0)
```

```sh
npm install
npm run dev      # chat at localhost:8787, counter at /counter.html
spago test       # same objects, in-memory simulator
```

Library in `src/Cloudflare/`, agents in `src/Ai/`, chat in `src/Chat/`, pages in `src/Frontend/`.

## Field guide

Where each idea lives, for reading later.

| Idea | Where |
| --- | --- |
| Row types as contracts: methods and events are rows, `Variant events` on the wire | `Cloudflare.Durable.Core`, `Events` |
| `Product (Const plan) (ReaderT env m)`: an applicative that plans before it runs | `Cloudflare.Static` |
| A monoid lifted through an applicative (`lift2 append`) | `Static`, `Runtime`, `Worker` |
| A record of monoids is a monoid (`Launch`, `Plan`) | `Runtime`, `Init` |
| `Alt` on `MaybeT`: the first source that answers | `Worker.Route`, `Chat.Room.Live.open` |
| `Contravariant`: `cmap` narrows a channel | `Cloudflare.Durable.Sockets` |
| `Divisible` / `Decidable` via `Op`: parameters encode contravariantly | `Cloudflare.Durable.Sql` |
| `Profunctor`: input side contravariant, output side covariant | `Sql.Statement`, `Ai.Agent.Def` |
| A Kleisli arrow (`Star (ExceptT e m)`) as a `Category`, `Strong`, `Choice` | `Ai.Agent.Agent` |
| `Bifunctor`: map the error or the value | `Cloudflare.Durable.Rpc` |
| `Void` as the error of a call that cannot fail; `absurd` lifts it | `Rpc.NoError`, `Rpc.infallible` |
| `Invariant`: a codec and a schema from one description | `Ai.Schema` |
| An existential hides a tool's types so a toolkit is an array | `Ai.Tool` |
| Natural transformations (`m ~> n`) move a model or tool between monads | `Ai.Model.hoist`, `Ai.Tool.hoist`, `Runtime.rethrow` |
| A `Maybe` with reasons is a monad (`Signal`) | `Cloudflare.Durable.Events` |
| `MonadRec` / `tailRecM`: loops that cannot blow the stack | `Container.ensure`, `Ai.Agent.mount` |
| `prismaticCodec`: a codec from a partial isomorphism | `Chat.Room.tagged` |
| Lenses and prisms compose with `<<<` into one screen's state | `Frontend.Chat` |
| Left-biased `Map.union`: what we hold wins over a reload | `Frontend.Chat` |
