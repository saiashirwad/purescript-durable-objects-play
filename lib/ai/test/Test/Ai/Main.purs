module Test.Ai.Main where

import Prelude

import Ai (AiError(..), Finish(..), Message(..), ModelId(..), invoke, mount, structured, text, tool, user)
import Ai.Catalogue as Catalogue
import Ai.Model as Model
import Ai.Provider (Auth(..))
import Ai.Provider as Provider
import Ai.Schema as Schema
import Ai.Wire.OpenAi as OpenAi
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref

main :: Effect Unit
main = launchAff_ do
  check "an agent runs the tool loop: call, result, answer" do
    called <- liftEffect $ Ref.new 0
    model <- Model.scripted
      [ { message: Assistant { text: Nothing, toolCalls: [ { id: "c1", name: "members", arguments: J.jsonEmptyObject } ] }, finish: ToolCalls, usage: Nothing }
      , { message: Assistant { text: Just "hi ann", toolCalls: [] }, finish: Stop, usage: Nothing }
      ]
    let
      members = tool "members" "who is here" (Schema.object {}) (Schema.array Schema.string) \_ -> do
        liftEffect $ Ref.modify_ (_ + 1) called
        pure [ "ann" ]
      greeter = mount model [ members ] $ text "Greet whoever is here."
    answer <- invoke greeter "hello?"
    calls <- liftEffect $ Ref.read called
    pure $ answer == Right "hi ann" && calls == 1

  check "structured agents decode the schema; agents compose as a Category" do
    model <- Model.scripted
      [ { message: Assistant { text: Just "{\"n\": 3}", toolCalls: [] }, finish: Stop, usage: Nothing }
      , { message: Assistant { text: Just "three", toolCalls: [] }, finish: Stop, usage: Nothing }
      ]
    let
      counter = mount model [] $ structured "Count." (Schema.object { n: Schema.int })
      namer = mount model [] $ text "Name the number."
      workflow = counter >>> lcmap (\r -> show r.n) namer
    answer <- invoke workflow "how many?"
    pure $ answer == Right "three"

  check "duplicate tool names are rejected at mount" do
    model <- Model.scripted []
    let
      t = tool "x" "x" (Schema.object {}) Schema.string \_ -> pure ""
      bad = mount model [ t, t ] $ text ""
    answer <- invoke bad ""
    pure $ answer == Left (Misconfigured "duplicate tool names: [\"x\",\"x\"]")

  check "a provider is data: url, auth and wire compose into one request" do
    seen <- liftEffect $ Ref.new Nothing
    let
      canned = jsonParser """{"choices":[{"message":{"role":"assistant","content":"pong"},"finish_reason":"stop"}],"usage":{"prompt_tokens":3,"completion_tokens":1}}"""
      post request = do
        liftEffect $ Ref.write (Just request) seen
        pure { status: 200, body: either (const J.jsonNull) identity canned }
      acme = Provider.openAiCompatible "acme" "https://llm.acme.test/v1"
      model = Provider.modelWith post acme "k-123" (Catalogue.unlisted "acme" (ModelId "acme-1"))
      blind = Provider.modelWith post acme "k-123" (Catalogue.unlisted "acme" (ModelId "acme-1")) { tools = false }
    reply <- Model.complete model { prompt: user "ping", tools: [], jsonOnly: false }
    refused <- Model.complete blind { prompt: user "ping", tools: [ J.jsonEmptyObject ], jsonOnly: false }
    request <- liftEffect $ Ref.read seen
    pure $ (request <#> _.url) == Just "https://llm.acme.test/v1/chat/completions"
      && (request <#> _.headers) == Just [ { name: "authorization", value: "Bearer k-123" } ]
      && (request <#> CA.decode OpenAi.request <<< _.body) == Just (Right { model: "acme-1", messages: [ User "ping" ], tools: Nothing, response_format: Nothing })
      && reply == Right { message: Assistant { text: Just "pong", toolCalls: [] }, finish: Stop, usage: Just { prompt: 3, completion: 1 } }
      && refused == Left (Misconfigured "acme/acme-1 cannot call tools")
      && Provider.authorize (Header "x-api-key") "k" { url: "u", headers: [] } == { url: "u", headers: [ { name: "x-api-key", value: "k" } ] }
      && Provider.authorize (Query "key") "k" { url: "u", headers: [] } == { url: "u?key=k", headers: [] }

  check "the openai wire round-trips every message shape, tool arguments included" do
    let
      transcript =
        [ System "be brief"
        , User "hi"
        , Assistant { text: Nothing, toolCalls: [ { id: "c1", name: "members", arguments: J.jsonEmptyObject } ] }
        , ToolResult { callId: "c1", content: "[\"ann\"]" }
        , Assistant { text: Just "just ann", toolCalls: [] }
        ]
    pure $ traverse (CA.decode OpenAi.message <<< CA.encode OpenAi.message) transcript == Right transcript

  log "All AI tests passed."

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name
