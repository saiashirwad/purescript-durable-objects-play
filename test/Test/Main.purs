module Test.Main where

import Prelude

import Chat.Room (PostError(..), RoomEvents)
import Chat.Room.Live (roomLive)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (Rpc, RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Simulator as Simulator
import Cloudflare.Durable.Runtime as Runtime
import Cloudflare.Durable (Signal(..))
import Data.Variant (Variant, match)
import Data.Either (Either(..))
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
import Example.Echo (echoLive)
import Cloudflare.Worker as Worker
import Cloudflare.Durable.Runtime (Launch(..))
import Data.Map as Map
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
    first <- chat.post { author: "ann", text: "hello" }
    second <- chat.post { author: "bob", text: "hi" }
    history <- Rpc.infallible $ chat.history unit
    pure $ (_.id <$> history) == [ 1, 2 ] && first.id == 1 && second.author == "bob"

  check "a blank post is a domain error" do
    let chat = Durable.getByName rooms "validation"
    result <- Rpc.run $ chat.post { author: "ann", text: "   " }
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
    _ <- Rpc.run $ chat.post { author: "ann", text: "hi" }
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
    _ <- byId.post { author: "ann", text: "only here" }
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
    , launched: \(Launch l) -> liftEffect $ Ref.write l.env launched
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
      && during == Right true && Map.lookup "GREETING" env == Just "hello from purescript"
      && early == Right true && after == Right false

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

  log "All tests passed."

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
    , joined: ("joined " <> _)
    , left: ("left " <> _)
    , typing: ("typing " <> _)
    }
