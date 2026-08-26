-- | The test objects hosted for real, so `wrangler dev` can exercise alarms
-- | and SQL on workerd.
module Test.Worker
  ( worker
  ) where

import Prelude

import Cloudflare.Durable as Durable
import Cloudflare.Durable.Http as Http
import Cloudflare.Worker (Worker)
import Cloudflare.Worker as Worker
import Test.Journal (journalLive)
import Test.Reminder (reminderLive)

worker :: Worker
worker =
  Worker.make (Http.route "/rpc" <$> Durable.host reminderLive)
    <> Worker.make (Http.route "/rpc" <$> Durable.host journalLive)
