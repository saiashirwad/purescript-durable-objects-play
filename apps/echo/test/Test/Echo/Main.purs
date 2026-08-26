module Test.Echo.Main where

import Prelude

import Cloudflare.Durable as Durable
import Cloudflare.Durable.Container as Container
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Simulator as Simulator
import Cloudflare.Worker as Worker
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Echo.Object (echoLive)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref

main :: Effect Unit
main = launchAff_ do
  timeline <- Simulator.clock
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

  log "All echo tests passed."

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name
