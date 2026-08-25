module Test.Main where

import Prelude

import Chat.Room (PostError(..))
import Chat.Room.Live (roomLive)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (Rpc, RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Simulator as Simulator
import Cloudflare.Durable.Runtime as Runtime
import Data.Either (Either(..))
import Effect (Effect)
import Data.Array (length)
import Data.String (take)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Example.Counter (counter, counterLive)
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
    Durable.manifest counterLive == { className: "Counter", methods: [ "get", "increment" ], variables: [] }

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

  check "since waits for the next post" do
    let chat = Durable.getByName rooms "waiting"
    _ <- Rpc.run $ chat.post { author: "ann", text: "before" }
    waiter <- forkAff $ Rpc.run $ chat.since 1
    delay $ Milliseconds 20.0
    _ <- Rpc.run $ chat.post { author: "bob", text: "after" }
    woken <- joinFiber waiter
    pure case woken of
      Right [ message ] -> message.text == "after" && message.id == 2
      _ -> false

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
