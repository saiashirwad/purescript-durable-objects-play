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
  in pure $ Durable.handlers { remind: \{ after, note } -> ... }
    `Durable.withHooks`
      (Durable.alarmHook $ Storage.get state noteKey >>= traverse_ \note ->
        Storage.put state (fired `Storage.at` "0") note)

-- Inside the object: sockets are a Contravariant channel. `cmap` narrows it
-- to one event; `connect`/`disconnect` hooks are monoids like `alarm`.
roomLive = Durable.implementWith room $ map handlersFor <$> open
  where
  open = ado
    state <- Durable.state
    sockets <- Durable.sockets room
    in do ...
  handlersFor r = Durable.handlers { post: post r, ... }
    `Durable.withHooks`
      (Durable.connectHook $ Sockets.broadcast
        (cmap (inj (Proxy :: _ "joined")) sockets) <<< _.tag)

-- A container is declared where it is used; wrangler config follows.
echoLive = Durable.implementWith echo ado
  state <- Durable.state
  box <- Durable.container (Container.image "./containers/echo/Dockerfile")
  in pure $ Durable.handlers { ... }
    `Durable.withHooks`
      ( Durable.fetchHook (\request -> do
          Container.ensure box 8080 $ Container.env [ "GREETING" /\ "hi" ] <> Container.noInternet
          Container.renew state box (Minutes 5.0)
          Just <$> Container.request box 8080 request)
          <> Durable.alarmHook (Container.expire state box)
      )

-- Agents: a Def names no model; mount attaches one and some tools. A model
-- is a provider (data: url, auth, wire) plus a catalogue entry (data: what
-- it can do); one wire serves every OpenAI-compatible provider.
flash :: Model Aff
flash = Provider.model Provider.deepseek key Catalogue.deepseekFlash

assistant :: Agent Runtime String String
assistant = mount (Model.hoist liftAff flash) [ whoIsHere ] $ text "You are terse."

-- In tests, time is a Clock you advance; due alarms fire during `advance`.
clock <- Simulator.clock
reminders <- Simulator.simulateOn clock reminderLive
Simulator.advance clock (Milliseconds 1000.0)
```

```sh
npm install
npm run dev       # chat at localhost:8787, counter at /counter.html
npm run build:ui  # build the UI library
npm run test:ui   # test style rules and keyboard state
npm run test:e2e  # test pages in Chrome with axe
spago test        # same objects, in-memory simulator
```

Libraries are in `lib/`; applications are in `apps/`.

The UI library is the `ui` package in `lib/ui/`. Open `/ui.html` during local
development to see its component lab. See `lib/ui/README.md` for its API,
theme model, accessibility rules, and package split plan.
