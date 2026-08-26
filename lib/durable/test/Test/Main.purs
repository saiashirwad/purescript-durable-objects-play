module Test.Main where

import Prelude

import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (Rpc, RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Runtime as Runtime
import Cloudflare.Durable.Simulator as Simulator
import Data.Either (Either(..))
import Data.String (take)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Test.Journal (journalLive)
import Test.Ledger (LedgerError(..), ledgerLive)
import Test.Reminder (reminderLive)

main :: Effect Unit
main = launchAff_ do
  ledgers <- Simulator.simulate ledgerLive

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

  log "All durable tests passed."

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name

succeeds :: forall e. Show e => Rpc e Boolean -> Aff Boolean
succeeds call = Rpc.run call >>= case _ of
  Right passed -> pure passed
  Left failure -> liftEffect $ throw $ "unexpected failure: " <> show failure
