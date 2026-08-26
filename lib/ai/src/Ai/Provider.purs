-- | A provider is data: a base URL, how it wants the key, and which wire it
-- | speaks. `model` turns one plus a key and a catalogue entry into a
-- | `Model Aff`. Adding a provider that speaks an existing wire is one line.
module Ai.Provider
  ( Auth(..)
  , Endpoint
  , Key
  , Provider
  , authorize
  , deepseek
  , groq
  , mistral
  , model
  , modelWith
  , ollama
  , openAiCompatible
  , openai
  , openrouter
  , together
  , xai
  ) where

import Prelude

import Ai.Catalogue (ModelInfo)
import Ai.Http (Header)
import Ai.Http as Http
import Ai.Model (AiError(..), Model(..))
import Ai.Wire (Wire)
import Ai.Wire.OpenAi as OpenAi
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError, withExceptT)
import Data.Argonaut.Core as J
import Data.Array (null, snoc)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Newtype (unwrap)
import Effect.Aff (Aff, attempt, message)

type Key = String

-- | Where the key goes.
data Auth
  = Bearer
  | Header String
  | Query String

derive instance eqAuth :: Eq Auth

type Provider = { name :: String, baseUrl :: String, auth :: Auth, wire :: Wire }

type Endpoint = { url :: String, headers :: Array Header }

authorize :: Auth -> Key -> Endpoint -> Endpoint
authorize auth key endpoint = case auth of
  Bearer -> endpoint { headers = snoc endpoint.headers { name: "authorization", value: "Bearer " <> key } }
  Header name -> endpoint { headers = snoc endpoint.headers { name, value: key } }
  Query name -> endpoint { url = endpoint.url <> "?" <> name <> "=" <> key }

model :: Provider -> Key -> ModelInfo -> Model Aff
model = modelWith Http.post

-- | `model`, with the network call passed in; tests capture it. A request
-- | the catalogue says the model cannot serve fails before it is sent.
modelWith :: Http.Post -> Provider -> Key -> ModelInfo -> Model Aff
modelWith post provider key info = Model \completion -> runExceptT do
  when (not info.tools && not (null completion.tools)) $ throwError $ cannot "call tools"
  when (not info.json && completion.jsonOnly) $ throwError $ cannot "answer in json"
  let endpoint = authorize provider.auth key { url: provider.baseUrl <> provider.wire.path, headers: [] }
  { status, body } <- withExceptT (Transport <<< message) $ ExceptT $ attempt $ post
    { url: endpoint.url, headers: endpoint.headers, body: provider.wire.encode { model: info.id, completion } }
  when (status /= 200) $ throwError $ Rejected { status, body: J.stringify body }
  except $ lmap (BadReply <<< CA.printJsonDecodeError) $ provider.wire.decode body
  where
  cannot what = Misconfigured $ provider.name <> "/" <> unwrap info.id <> " cannot " <> what

-- Providers ---------------------------------------------------------------------

openAiCompatible :: String -> String -> Provider
openAiCompatible name baseUrl = { name, baseUrl, auth: Bearer, wire: OpenAi.wire }

deepseek :: Provider
deepseek = openAiCompatible "deepseek" "https://api.deepseek.com"

openai :: Provider
openai = openAiCompatible "openai" "https://api.openai.com/v1"

groq :: Provider
groq = openAiCompatible "groq" "https://api.groq.com/openai/v1"

mistral :: Provider
mistral = openAiCompatible "mistral" "https://api.mistral.ai/v1"

xai :: Provider
xai = openAiCompatible "xai" "https://api.x.ai/v1"

openrouter :: Provider
openrouter = openAiCompatible "openrouter" "https://openrouter.ai/api/v1"

together :: Provider
together = openAiCompatible "together" "https://api.together.xyz/v1"

-- | Local; any key will do.
ollama :: Provider
ollama = openAiCompatible "ollama" "http://localhost:11434/v1"
