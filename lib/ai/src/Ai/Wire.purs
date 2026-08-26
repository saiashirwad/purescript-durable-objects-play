-- | A wire format: how a completion becomes a request body and a response
-- | body becomes a `Reply`. There are few of these (OpenAI chat completions,
-- | Anthropic messages, ...) and many providers speak each one.
module Ai.Wire
  ( Request
  , Wire
  ) where

import Ai.Model (Completion, ModelId, Reply)
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (JsonDecodeError)
import Data.Either (Either)

type Request = { model :: ModelId, completion :: Completion }

type Wire =
  { name :: String
  , path :: String
  , encode :: Request -> Json
  , decode :: Json -> Either JsonDecodeError Reply
  }
