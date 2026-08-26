module Counter.Demo where

import Prelude

import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Simulator as Simulator
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Counter.Object (counterLive)

main :: Effect Unit
main = launchAff_ do
  counters <- Simulator.simulate counterLive
  let user = Durable.getByName counters "user-123"
  result <- Rpc.run do
    first <- user.increment unit
    second <- user.increment unit
    value <- user.get unit
    pure { first, second, value }
  case result of
    Right { first, second, value } -> do
      log $ "increment results: " <> show first <> ", " <> show second
      log $ "stored value: " <> show value
    Left failure -> log $ "The counter failed: " <> show failure
  log $ "manifest: " <> show (Durable.manifest counterLive)
