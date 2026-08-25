module Test.Main where

import Prelude

import Chat.Room (PostError(..), ReactError(..), RoomEvents)
import Chat.Markdown (Block(..), Inline(..))
import Chat.Markdown as Markdown
import Data.Array as Array
import Chat.Room.Live (roomLive, roomLiveWith)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Core (Live(..))
import Cloudflare.Durable.Rpc (Rpc, RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Simulator as Simulator
import Cloudflare.Durable.Runtime as Runtime
import Cloudflare.Durable (Signal(..))
import Data.Variant (Variant, match)
import Data.Either (Either(..), either)
import Data.Traversable (traverse)
import Effect (Effect)
import Data.Array (length)
import Data.String (take)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Ref as Ref
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Example.Counter (counter, counterLive)
import Ai (AiError(..), Finish(..), Message(..), ModelId(..), invoke, mount, structured, text, tool, user)
import Ai.Catalogue as Catalogue
import Ai.Provider (Auth(..))
import Ai.Provider as Provider
import Ai.Wire.OpenAi as OpenAi
import Data.Codec.Argonaut as CA
import Data.Argonaut.Parser (jsonParser)
import Ai.Model as Model
import Ai.Schema as Schema
import Data.Argonaut.Core as J
import Example.Echo (echoLive)
import Cloudflare.Worker as Worker
import Cloudflare.Durable.Container as Container
import Data.Map as Map
import Data.Profunctor (lcmap)
import Test.Journal (journalLive)
import Test.Ledger (LedgerError(..), ledgerLive)
import Test.Reminder (reminderLive)

main :: Effect Unit
main = launchAff_ do
  counters <- Simulator.simulate counterLive
  ledgers <- Simulator.simulate ledgerLive

  check "each name is its own instance" $ succeeds do
    let alice = Durable.getByName counters "alice"
    let bob = Durable.getByName counters "bob"
    _ <- alice.increment unit
    a <- alice.increment unit
    b <- bob.increment unit
    pure $ { a, b } == { a: 2, b: 1 }

  check "an id addresses the same instance as its name" $ succeeds do
    let byName = Durable.getByName counters "carol"
    let byId = Durable.get counters (Durable.idFromName counters "carol")
    _ <- byName.increment unit
    value <- byId.get unit
    pure $ value == 1

  check "the manifest describes the object" $ pure $
    Durable.manifest counterLive == { className: "Counter", methods: [ "get", "increment" ], variables: [], container: Nothing }

  check "a domain error crosses the wire intact" do
    let account = Durable.getByName ledgers "dave"
    result <- Rpc.run do
      _ <- Rpc.infallible $ account.deposit 10
      account.withdraw 25
    pure $ result == Left (DomainError (Insufficient { balance: 10, requested: 25 }))

  check "a platform error reaches the caller as PlatformError" do
    let account = Durable.getByName ledgers "erin"
    _ <- Rpc.run $ account.corrupt unit
    result <- Rpc.run $ account.balance unit
    pure case result of
      Left (PlatformError (Runtime.PlatformError { operation: "storage.get \"balance\"" })) -> true
      _ -> false

  rooms <- Simulator.simulate roomLive

  check "a room keeps its messages in order" $ succeeds do
    id <- liftAff $ Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    first <- chat.post { author: "ann", text: "hello", images: [], replyTo: Nothing }
    second <- chat.post { author: "bob", text: "hi", images: [], replyTo: Nothing }
    history <- Rpc.infallible $ chat.history unit
    pure $ (_.id <$> history) == [ 1, 2 ] && first.id == 1 && second.author == "bob"

  check "a blank post is a domain error" do
    let chat = Durable.getByName rooms "validation"
    result <- Rpc.run $ chat.post { author: "ann", text: "   ", images: [], replyTo: Nothing }
    pure $ result == Left (DomainError TextRequired)

  check "sockets get posts and presence; members tracks them" do
    id <- Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    annLog <- liftEffect $ Ref.new []
    bobLog <- liftEffect $ Ref.new []
    let record log signal = Ref.modify_ (_ <> [ describe signal ]) log
    _ <- liftEffect $ Durable.listen rooms id "ann" (record annLog)
    stopBob <- liftEffect $ Durable.listen rooms id "bob" (record bobLog)
    _ <- Rpc.run $ chat.typing "bob"
    _ <- Rpc.run $ chat.post { author: "ann", text: "hi", images: [], replyTo: Nothing }
    members <- Rpc.run $ chat.members unit
    liftEffect stopBob
    after <- Rpc.run $ chat.members unit
    ann <- liftEffect $ Ref.read annLog
    bob <- liftEffect $ Ref.read bobLog
    pure $ ann == [ "opened", "joined ann", "joined bob", "typing bob", "message hi", "left bob" ]
      && bob == [ "opened", "joined bob", "typing bob", "message hi", "closed" ]
      && members == Right [ "ann", "bob" ]
      && after == Right [ "ann" ]

  check "unique ids do not collide with names" $ succeeds do
    id <- liftAff $ Durable.newUniqueId rooms
    let byId = Durable.get rooms id
    let byName = Durable.getByName rooms (Durable.idToString id)
    _ <- byId.post { author: "ann", text: "only here", images: [], replyTo: Nothing }
    other <- Rpc.infallible $ byName.history unit
    pure $ length other == 0 && Just id == Just (Durable.idFromString rooms (Durable.idToString id))

  check "loopback = connect after serve behaves as the implementation" do
    cell <- liftEffect $ Ref.new 0
    let
      impl =
        { increment: \_ -> liftEffect $ Ref.modify (_ + 1) cell
        , get: \_ -> liftEffect $ Ref.read cell
        }
      wired = Durable.loopback counter impl
    viaWire <- Rpc.run $ wired.increment unit
    direct <- Rpc.run $ impl.increment unit
    both <- Rpc.run $ wired.get unit
    pure $ viaWire == Right 1 && direct == Right 2 && both == Right 2

  timeline <- Simulator.clock
  reminders <- Simulator.simulateOn timeline reminderLive

  check "an alarm fires once the clock passes it" do
    let bell = Durable.getByName reminders "bell"
    _ <- Rpc.run $ bell.remind { after: 1000.0, note: "stand up" }
    before <- Rpc.run $ bell.pending unit
    Simulator.advance timeline (Milliseconds 999.0)
    early <- Rpc.run $ bell.fired unit
    Simulator.advance timeline (Milliseconds 1.0)
    late <- Rpc.run $ bell.fired unit
    after <- Rpc.run $ bell.pending unit
    pure $ before == Right true && early == Right [] && late == Right [ "stand up" ] && after == Right false

  check "list sees only its prefix, in key order, and deleteAll clears it" $ succeeds do
    let bell = Durable.getByName reminders "list"
    _ <- bell.remind { after: 0.0, note: "first" }
    liftAff $ Simulator.advance timeline (Milliseconds 0.0)
    _ <- bell.remind { after: 0.0, note: "second" }
    liftAff $ Simulator.advance timeline (Milliseconds 0.0)
    both <- bell.fired unit
    _ <- bell.forget unit
    none <- bell.fired unit
    pure $ both == [ "first", "second" ] && none == []

  journals <- Simulator.simulate journalLive

  launched <- liftEffect $ Ref.new Map.empty
  echoes <- Simulator.simulateWith
    { serve: \port request -> pure $ Worker.text 200 $ "port " <> show port <> " got " <> Worker.pathname request
    , launched: liftEffect <<< flip Ref.write launched <<< Container.environment
    , variables: Map.empty
    }
    timeline
    echoLive

  check "a container starts on first request, proxies it, and stops when idle" do
    let id = Durable.idFromName echoes "box"
    let stub = Durable.get echoes id
    before <- Rpc.run $ stub.running unit
    response <- Durable.http echoes id (Worker.requestTo "http://echo/hello")
    body <- Worker.responseText response
    during <- Rpc.run $ stub.running unit
    env <- liftEffect $ Ref.read launched
    Simulator.advance timeline (Milliseconds 299000.0)
    early <- Rpc.run $ stub.running unit
    Simulator.advance timeline (Milliseconds 2000.0)
    after <- Rpc.run $ stub.running unit
    pure $ before == Right false && Worker.status response == 200 && body == "port 8080 got /hello"
      && during == Right true
      && Map.lookup "GREETING" env == Just "hello from purescript"
      && early == Right true
      && after == Right false

  check "sql rows decode through the applicative row" $ succeeds do
    let book = Durable.getByName journals "book"
    _ <- book.record { account: "rent", amount: 700 }
    _ <- book.record { account: "rent", amount: -200 }
    _ <- book.record { account: "food", amount: 45 }
    rent <- book.balance "rent"
    rows <- book.entries "rent"
    empty <- book.balance "nobody"
    pure $ rent == 500 && rows == [ { id: 1, amount: 700 }, { id: 2, amount: -200 } ] && empty == 0

  check "a row that fails to decode is a PlatformError naming the statement" do
    let book = Durable.getByName journals "book"
    result <- Rpc.run $ book.mistype "rent"
    pure case result of
      Left (PlatformError (Runtime.PlatformError { operation })) -> take 3 operation == "sql"
      _ -> false

  check "deleteAll drops sql tables too" $ succeeds do
    let book = Durable.getByName journals "book"
    _ <- book.reset unit
    gone <- book.entries "rent"
    pure $ gone == []

  check "an agent runs the tool loop: call, result, answer" do
    called <- liftEffect $ Ref.new 0
    model <- Model.scripted
      [ { message: Assistant { text: Nothing, toolCalls: [ { id: "c1", name: "members", arguments: J.jsonEmptyObject } ] }, finish: ToolCalls, usage: Nothing }
      , { message: Assistant { text: Just "hi ann", toolCalls: [] }, finish: Stop, usage: Nothing }
      ]
    let
      members = tool "members" "who is here" (Schema.object {}) (Schema.array Schema.string) \_ -> do
        liftEffect $ Ref.modify_ (_ + 1) called
        pure [ "ann" ]
      greeter = mount model [ members ] $ text "Greet whoever is here."
    answer <- invoke greeter "hello?"
    calls <- liftEffect $ Ref.read called
    pure $ answer == Right "hi ann" && calls == 1

  check "structured agents decode the schema; agents compose as a Category" do
    model <- Model.scripted
      [ { message: Assistant { text: Just "{\"n\": 3}", toolCalls: [] }, finish: Stop, usage: Nothing }
      , { message: Assistant { text: Just "three", toolCalls: [] }, finish: Stop, usage: Nothing }
      ]
    let
      counter' = mount model [] $ structured "Count." (Schema.object { n: Schema.int })
      namer = mount model [] $ text "Name the number."
      workflow = counter' >>> lcmap (\r -> show r.n) namer
    answer <- invoke workflow "how many?"
    pure $ answer == Right "three"

  check "duplicate tool names are rejected at mount" do
    model <- Model.scripted []
    let
      t = tool "x" "x" (Schema.object {}) Schema.string \_ -> pure ""
      bad = mount model [ t, t ] $ text ""
    answer <- invoke bad ""
    pure $ answer == Left (Misconfigured "duplicate tool names: [\"x\",\"x\"]")

  check "a provider is data: url, auth and wire compose into one request" do
    seen <- liftEffect $ Ref.new Nothing
    let
      canned = jsonParser """{"choices":[{"message":{"role":"assistant","content":"pong"},"finish_reason":"stop"}],"usage":{"prompt_tokens":3,"completion_tokens":1}}"""
      post request = do
        liftEffect $ Ref.write (Just request) seen
        pure { status: 200, body: either (const J.jsonNull) identity canned }
      acme = Provider.openAiCompatible "acme" "https://llm.acme.test/v1"
      model = Provider.modelWith post acme "k-123" (Catalogue.unlisted "acme" (ModelId "acme-1"))
      blind = Provider.modelWith post acme "k-123" (Catalogue.unlisted "acme" (ModelId "acme-1")) { tools = false }
    reply <- Model.complete model { prompt: user "ping", tools: [], jsonOnly: false }
    refused <- Model.complete blind { prompt: user "ping", tools: [ J.jsonEmptyObject ], jsonOnly: false }
    request <- liftEffect $ Ref.read seen
    pure $ (request <#> _.url) == Just "https://llm.acme.test/v1/chat/completions"
      && (request <#> _.headers) == Just [ { name: "authorization", value: "Bearer k-123" } ]
      && (request <#> CA.decode OpenAi.request <<< _.body) == Just (Right { model: "acme-1", messages: [ User "ping" ], tools: Nothing, response_format: Nothing })
      && reply == Right { message: Assistant { text: Just "pong", toolCalls: [] }, finish: Stop, usage: Just { prompt: 3, completion: 1 } }
      && refused == Left (Misconfigured "acme/acme-1 cannot call tools")
      && Provider.authorize (Header "x-api-key") "k" { url: "u", headers: [] } == { url: "u", headers: [ { name: "x-api-key", value: "k" } ] }
      && Provider.authorize (Query "key") "k" { url: "u", headers: [] } == { url: "u?key=k", headers: [] }

  check "the openai wire round-trips every message shape, tool arguments included" do
    let
      transcript =
        [ System "be brief"
        , User "hi"
        , Assistant { text: Nothing, toolCalls: [ { id: "c1", name: "members", arguments: J.jsonEmptyObject } ] }
        , ToolResult { callId: "c1", content: "[\"ann\"]" }
        , Assistant { text: Just "just ann", toolCalls: [] }
        ]
    pure $ traverse (CA.decode OpenAi.message <<< CA.encode OpenAi.message) transcript == Right transcript

  check "a post mentioning @ai is answered from the alarm, with a tool call" do
    model <- Model.scripted
      [ { message: Assistant { text: Nothing, toolCalls: [ { id: "c1", name: "members", arguments: J.jsonEmptyObject } ] }, finish: ToolCalls, usage: Nothing }
      , { message: Assistant { text: Just "hello ann, just us two", toolCalls: [] }, finish: Stop, usage: Nothing }
      ]
    bots <- Simulator.simulateWith
      (Simulator.noContainer { variables = Map.singleton "DEEPSEEK_API_KEY" "test-key" })
      timeline
      (roomLiveWith \_ -> model)
    id <- Durable.newUniqueId bots
    logs <- liftEffect $ Ref.new []
    _ <- liftEffect $ Durable.listen bots id "ann" \signal -> Ref.modify_ (_ <> [ describe signal ]) logs
    _ <- Rpc.run $ (Durable.get bots id).post { author: "ann", text: "hey @ai, who is here?", images: [], replyTo: Nothing }
    Simulator.advance timeline (Milliseconds 0.0)
    seen <- liftEffect $ Ref.read logs
    history <- Rpc.run $ (Durable.get bots id).history unit
    pure $ (seen == [ "opened", "joined ann", "message hey @ai, who is here?", "typing ai", "message hello ann, just us two" ])
      && ((map _.author <$> history) == Right [ "ann", "ai" ])

  check "markdown parses blocks and inlines, and finds mentions" $ pure $
    Markdown.parse "# Hi @bob\nsee **this** and `x` at https://a.io/p.\n\n> quoted\n- one\n- two\n```purs\nmain = 1\n```"
      ==
        [ Heading 1 [ Text "Hi ", Mention "bob" ]
        , Paragraph [ Text "see ", Bold [ Text "this" ], Text " and ", InlineCode "x", Text " at ", Link { text: "https://a.io/p", url: "https://a.io/p" }, Text "." ]
        , Quote [ Paragraph [ Text "quoted" ] ]
        , Bullets [ [ Text "one" ], [ Text "two" ] ]
        , Code (Just "purs") "main = 1"
        ]
      && Markdown.mentions "@ann and @ann, cc @bob but not a@b.c" == [ "ann", "bob", "b.c" ]

  check "replies must point at a real message; mentions are recorded" do
    let chat = Durable.getByName rooms "threads"
    first <- Rpc.run $ chat.post { author: "ann", text: "hello @bob", images: [], replyTo: Nothing }
    bad <- Rpc.run $ chat.post { author: "bob", text: "??", images: [], replyTo: Just 99 }
    good <- Rpc.run $ chat.post { author: "bob", text: "hi!", images: [], replyTo: Just 1 }
    missing <- Rpc.run $ chat.post { author: "bob", text: "pic", images: [ 42 ], replyTo: Nothing }
    pure $ (map _.mentions first == Right [ "bob" ])
      && bad == Left (DomainError (NoSuchReply 99))
      && (map _.replyTo good == Right (Just 1))
      && missing == Left (DomainError (NoSuchImage 42))

  check "reactions toggle per person and broadcast an update" do
    id <- Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    logs <- liftEffect $ Ref.new []
    _ <- liftEffect $ Durable.listen rooms id "ann" \signal -> Ref.modify_ (_ <> [ describe signal ]) logs
    _ <- Rpc.run $ chat.post { author: "ann", text: "vote", images: [], replyTo: Nothing }
    a <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "ann" }
    b <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "bob" }
    c <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "ann" }
    d <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "bob" }
    none <- Rpc.run $ chat.react { id: 7, emoji: "👍", by: "bob" }
    seen <- liftEffect $ Ref.read logs
    pure $ (map _.reactions a == Right [ { emoji: "👍", by: [ "ann" ] } ])
      && (map _.reactions b == Right [ { emoji: "👍", by: [ "ann", "bob" ] } ])
      && (map _.reactions c == Right [ { emoji: "👍", by: [ "bob" ] } ])
      && (map _.reactions d == Right [])
      && none == Left (DomainError (NoSuchMessage 7))
      && Array.drop 3 seen == [ "updated [\"👍×1\"]", "updated [\"👍×2\"]", "updated [\"👍×1\"]", "updated []" ]

  check "images round-trip through the room's fetch hook and validate on post" do
    id <- Durable.newUniqueId rooms
    let png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
    uploaded <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: png }
    body <- Worker.responseText uploaded
    rejected <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "text/plain", base64: "aGk=" }
    served <- Durable.http rooms id $ Worker.requestTo "http://room/image/1"
    missing <- Durable.http rooms id $ Worker.requestTo "http://room/image/9"
    posted <- Rpc.run $ (Durable.get rooms id).post { author: "ann", text: "", images: [ 1 ], replyTo: Nothing }
    pure $ Worker.status uploaded == 200 && body == "{\"id\":1}" && Worker.status rejected == 415
      && Worker.status served == 200
      && Worker.status missing == 404
      && (map _.images posted == Right [ 1 ])

  check "a matched fetch hook stops later hooks" do
    layered <- Simulator.simulate $ withLiveHooks
      (Durable.fetchHook \_ -> pure $ Just $ Worker.text 418 "later hook")
      roomLive
    id <- Durable.newUniqueId layered
    missing <- Durable.http layered id $ Worker.requestTo "http://room/image/9"
    missingBody <- Worker.responseText missing
    unmatched <- Durable.http layered id $ Worker.requestTo "http://room/other"
    pure $ Worker.status missing == 404
      && missingBody == "no such image"
      && Worker.status unmatched == 418

  log "All tests passed."

withLiveHooks
  :: forall name api events
   . Durable.Hooks
  -> Durable.Live name api events
  -> Durable.Live name api events
withLiveHooks extra (Live live) =
  Live (live { activate = map (map (_ `Durable.withHooks` extra)) live.activate })

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name

succeeds :: forall e. Show e => Rpc e Boolean -> Aff Boolean
succeeds call = Rpc.run call >>= case _ of
  Right passed -> pure passed
  Left failure -> liftEffect $ throw $ "unexpected failure: " <> show failure

describe :: Signal (Variant RoomEvents) -> String
describe = case _ of
  Opened -> "opened"
  Closed -> "closed"
  Garbled why -> "garbled " <> why
  Delivered event -> event # match
    { message: \m -> "message " <> m.text
    , updated: \m -> "updated " <> show (m.reactions <#> \r -> r.emoji <> "×" <> show (Array.length r.by))
    , joined: ("joined " <> _)
    , left: ("left " <> _)
    , typing: ("typing " <> _)
    }
