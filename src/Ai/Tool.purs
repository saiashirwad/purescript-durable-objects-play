-- | A tool is a typed function the model may call. Its parameter and result
-- | types are hidden behind `Tool m` (an existential), so a `Toolkit m` is a
-- | plain array and a monoid. `hoist` moves a tool between monads with a
-- | natural transformation `m ~> n`.
module Ai.Tool
  ( Tool
  , Toolkit
  , call
  , describe
  , hoist
  , name
  , tool
  ) where

import Prelude

import Ai.Prompt (ToolCall)
import Ai.Schema (Schema)
import Ai.Schema as Schema
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array (find)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object

newtype Tool m = Tool
  { name :: String
  , description :: String
  , parameters :: Json
  , run :: Json -> m (Either String Json)
  }

type Toolkit m = Array (Tool m)

-- | `tool "members" "Who is in the room" params result handler`.
tool :: forall m i o. Applicative m => String -> String -> Schema i -> Schema o -> (i -> m o) -> Tool m
tool toolName description params result handler = Tool
  { name: toolName
  , description
  , parameters: Schema.json params
  , run: \arguments -> case CA.decode (Schema.codec params) arguments of
      Left err -> pure $ Left $ "bad arguments: " <> CA.printJsonDecodeError err
      Right input -> Right <<< CA.encode (Schema.codec result) <$> handler input
  }

name :: forall m. Tool m -> String
name (Tool t) = t.name

hoist :: forall m n. (m ~> n) -> Tool m -> Tool n
hoist nat (Tool t) = Tool t { run = nat <<< t.run }

-- | The OpenAI-style description the API wants.
describe :: forall m. Tool m -> Json
describe (Tool t) = J.fromObject $ Object.fromFoldable
  [ "type" /\ J.fromString "function"
  , "function" /\ J.fromObject
      ( Object.fromFoldable
          [ "name" /\ J.fromString t.name
          , "description" /\ J.fromString t.description
          , "parameters" /\ t.parameters
          ]
      )
  ]

call :: forall m. Applicative m => Toolkit m -> ToolCall -> m (Either String Json)
call tools request = case find (\(Tool t) -> t.name == request.name) tools of
  Just (Tool t) -> t.run request.arguments
  Nothing -> pure $ Left $ "no such tool: " <> request.name
