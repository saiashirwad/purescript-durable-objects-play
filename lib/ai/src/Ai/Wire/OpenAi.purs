-- | The OpenAI chat-completions wire format, as codecs. Both directions are
-- | partial isomorphisms between our types and wire records; no JSON is
-- | built by hand. DeepSeek, Groq, Mistral, xAI, OpenRouter, Ollama and
-- | others all speak this.
module Ai.Wire.OpenAi
  ( WireRequest
  , embedded
  , message
  , request
  , toolCall
  , wire
  ) where

import Prelude

import Ai.Model (Finish(..), ModelId(..), Reply)
import Ai.Prompt (Message(..), ToolCall, messages)
import Ai.Wire (Request, Wire)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, null)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either, hush, note)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)

wire :: Wire
wire = { name: "openai-completions", path: "/chat/completions", encode, decode }

-- Requests ------------------------------------------------------------------

type WireRequest =
  { model :: String
  , messages :: Array Message
  , tools :: Maybe (Array Json)
  , response_format :: Maybe { "type" :: String }
  }

request :: JsonCodec WireRequest
request = CAR.object "Request"
  { model: CA.string
  , messages: CA.array message
  , tools: CAR.optional (CA.array CA.json)
  , response_format: CAR.optional (CAR.object "Format" { "type": CA.string })
  }

encode :: Request -> Json
encode { model: ModelId id, completion } = CA.encode request
  { model: id
  , messages: messages completion.prompt
  , tools: nonEmpty completion.tools
  , response_format: if completion.jsonOnly then Just { "type": "json_object" } else Nothing
  }

-- | JSON that arrives as a string inside JSON.
embedded :: JsonCodec Json
embedded = CA.prismaticCodec "embedded json" (hush <<< jsonParser) J.stringify CA.string

toolCall :: JsonCodec ToolCall
toolCall = dimap toWire fromWire $ CAR.object "Call"
  { id: CA.string
  , "type": CA.string
  , function: CAR.object "Function" { name: CA.string, arguments: embedded }
  }
  where
  toWire c = { id: c.id, "type": "function", function: { name: c.name, arguments: c.arguments } }
  fromWire w = { id: w.id, name: w.function.name, arguments: w.function.arguments }

-- | One wire record covers all four roles; the role picks the constructor.
message :: JsonCodec Message
message = CA.prismaticCodec "Message" fromWire toWire $ CAR.object "Message"
  { role: CA.string
  , content: Compat.maybe CA.string
  , tool_calls: CAR.optional (CA.array toolCall)
  , tool_call_id: CAR.optional CA.string
  }
  where
  blank = { role: "", content: Nothing, tool_calls: Nothing, tool_call_id: Nothing }
  toWire = case _ of
    System text -> blank { role = "system", content = Just text }
    User text -> blank { role = "user", content = Just text }
    Assistant { text, toolCalls } -> blank { role = "assistant", content = text, tool_calls = nonEmpty toolCalls }
    ToolResult { callId, content } -> blank { role = "tool", content = Just content, tool_call_id = Just callId }
  fromWire w = case w.role of
    "system" -> System <$> w.content
    "user" -> User <$> w.content
    "assistant" -> Just $ Assistant { text: w.content, toolCalls: fromMaybe [] w.tool_calls }
    "tool" -> ToolResult <$> ({ callId: _, content: _ } <$> w.tool_call_id <*> w.content)
    _ -> Nothing

nonEmpty :: forall a. Array a -> Maybe (Array a)
nonEmpty xs = if null xs then Nothing else Just xs

-- Replies -----------------------------------------------------------------------

type WireReply =
  { choices :: Array { message :: Message, finish_reason :: Maybe String }
  , usage :: Maybe { prompt_tokens :: Int, completion_tokens :: Int }
  }

reply :: JsonCodec WireReply
reply = CAR.object "Reply"
  { choices: CA.array $ CAR.object "Choice" { message, finish_reason: Compat.maybe CA.string }
  , usage: CAR.optional $ CAR.object "Usage" { prompt_tokens: CA.int, completion_tokens: CA.int }
  }

decode :: Json -> Either JsonDecodeError Reply
decode json = do
  wire' <- CA.decode reply json
  choice <- note (CA.AtKey "choices" CA.MissingValue) $ head wire'.choices
  pure
    { message: choice.message
    , finish: finish choice
    , usage: wire'.usage <#> \u -> { prompt: u.prompt_tokens, completion: u.completion_tokens }
    }
  where
  finish { message: m, finish_reason } = case finish_reason, m of
    Just "stop", _ -> Stop
    Just "tool_calls", _ -> ToolCalls
    Just "length", _ -> Length
    Just "content_filter", _ -> Filtered
    Just other, _ -> Other other
    Nothing, Assistant { toolCalls } | not (null toolCalls) -> ToolCalls
    Nothing, _ -> Stop
