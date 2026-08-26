module Test.Counter.Main where

import Prelude

import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Simulator as Simulator
import Counter.Object (counter, counterLive)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref

main :: Effect Unit
main = launchAff_ do
  counters <- Simulator.simulate counterLive

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

  log "All counter tests passed."

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name

succeeds :: forall e. Show e => Rpc.Rpc e Boolean -> Aff Boolean
succeeds call = Rpc.run call >>= case _ of
  Right passed -> pure passed
  Left failure -> liftEffect $ throw $ "unexpected failure: " <> show failure
