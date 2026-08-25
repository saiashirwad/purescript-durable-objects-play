-- | DeepSeek over its OpenAI-compatible chat completions API. The same
-- | encoding serves any OpenAI-compatible endpoint: pass another `url`.
module Ai.DeepSeek
  ( Config
  , flash
  , model
  , pro
  ) where

import Prelude

import Ai.Model (AiError(..), Model(..), Reply)
import Ai.Prompt (Message(..), ToolCall, messages)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, attempt, message)
import Foreign.Object as Object

foreign import postJson :: String -> String -> Json -> Effect (Promise { status :: Int, body :: Json })

type Config = { apiKey :: String, model :: String, url :: String }

flash :: String -> Config
flash apiKey = { apiKey, model: "deepseek-v4-flash", url: "https://api.deepseek.com/chat/completions" }

pro :: String -> Config
pro apiKey = (flash apiKey) { model = "deepseek-v4-pro" }

model :: Config -> Model Aff
model config = Model \completion -> do
  let
    body = J.fromObject $ Object.fromFoldable $
      [ "model" /\ J.fromString config.model
      , "messages" /\ J.fromArray (encodeMessage <$> messages completion.prompt)
      ]
        <> (if completion.tools == [] then [] else [ "tools" /\ J.fromArray completion.tools ])
        <> (if completion.jsonOnly then [ "response_format" /\ J.fromObject (Object.singleton "type" (J.fromString "json_object")) ] else [])
  outcome <- attempt $ toAffE $ postJson config.url config.apiKey body
  pure case outcome of
    Left err -> Left $ Transport $ message err
    Right { status, body: response }
      | status /= 200 -> Left $ Transport $ "HTTP " <> show status <> ": " <> J.stringify response
      | otherwise -> either (Left <<< BadReply <<< CA.printJsonDecodeError) Right $ decodeReply response

encodeMessage :: Message -> Json
encodeMessage = J.fromObject <<< Object.fromFoldable <<< case _ of
  System text -> [ "role" /\ J.fromString "system", "content" /\ J.fromString text ]
  User text -> [ "role" /\ J.fromString "user", "content" /\ J.fromString text ]
  Assistant { text, toolCalls } ->
    [ "role" /\ J.fromString "assistant", "content" /\ maybe J.jsonNull J.fromString text ]
      <> (if toolCalls == [] then [] else [ "tool_calls" /\ J.fromArray (encodeCall <$> toolCalls) ])
  ToolResult { callId, content } ->
    [ "role" /\ J.fromString "tool", "tool_call_id" /\ J.fromString callId, "content" /\ J.fromString content ]

encodeCall :: ToolCall -> Json
encodeCall call = J.fromObject $ Object.fromFoldable
  [ "id" /\ J.fromString call.id
  , "type" /\ J.fromString "function"
  , "function" /\ J.fromObject (Object.fromFoldable [ "name" /\ J.fromString call.name, "arguments" /\ J.fromString (J.stringify call.arguments) ])
  ]

type WireCall = { id :: String, function :: { name :: String, arguments :: String } }

type WireReply =
  { choices :: Array { message :: { content :: Maybe String, tool_calls :: Maybe (Array WireCall) }, finish_reason :: Maybe String }
  , usage :: Maybe { prompt_tokens :: Int, completion_tokens :: Int }
  }

replyCodec :: JsonCodec WireReply
replyCodec = CAR.object "Reply"
  { choices: CA.array $ CAR.object "Choice"
      { message: CAR.object "Message"
          { content: Compat.maybe CA.string
          , tool_calls: CAR.optional $ CA.array $ CAR.object "Call"
              { id: CA.string, function: CAR.object "Function" { name: CA.string, arguments: CA.string } }
          }
      , finish_reason: Compat.maybe CA.string
      }
  , usage: CAR.optional $ CAR.object "Usage" { prompt_tokens: CA.int, completion_tokens: CA.int }
  }

decodeReply :: Json -> Either JsonDecodeError Reply
decodeReply json = do
  wire <- CA.decode replyCodec json
  choice <- note (CA.AtKey "choices" CA.MissingValue) $ head wire.choices
  toolCalls <- traverse decodeCall $ fromMaybe [] choice.message.tool_calls
  pure
    { message: Assistant { text: choice.message.content, toolCalls }
    , finish: fromMaybe "stop" choice.finish_reason
    , usage: wire.usage <#> \u -> { prompt: u.prompt_tokens, completion: u.completion_tokens }
    }
  where
  decodeCall call = do
    arguments <- either (Left <<< CA.Named ("arguments of " <> call.function.name) <<< CA.UnexpectedValue <<< J.fromString) Right
      $ jsonParser call.function.arguments
    pure { id: call.id, name: call.function.name, arguments }
