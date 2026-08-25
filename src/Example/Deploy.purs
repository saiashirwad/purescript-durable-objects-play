-- | The deployment, as a value. `scripts/config.mjs` writes it to
-- | `wrangler.jsonc`.
module Example.Deploy
  ( config
  ) where

import Cloudflare.Worker as Worker
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import Example.Api (api)

config :: Json
config = Worker.wranglerConfig
  { name: "durable-mini"
  , main: "worker/index.js"
  , compatibilityDate: "2026-08-01"
  , assets: Just "./public"
  }
  api
