module Example.Deploy
  ( config
  , configWithContainers
  ) where

import Cloudflare.Worker as Worker
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))
import Example.Api (api)

-- | Container images need the Workers Paid plan, so they are declared only
-- | on request (`CONTAINERS=1`); the objects themselves always exist.
config :: Json
config = configFor false

configWithContainers :: Json
configWithContainers = configFor true

configFor :: Boolean -> Json
configFor containers = Worker.wranglerConfig
  { name: "durable-mini"
  , main: "worker/index.js"
  , compatibilityDate: "2026-08-01"
  , assets: Just "./public"
  , containers
  }
  api
