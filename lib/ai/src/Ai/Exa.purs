-- | Web search through Exa: a query in, a few pages with excerpts out.
module Ai.Exa
  ( Result
  , Search
  , search
  ) where

import Prelude

import Ai.Http as Http
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError, withExceptT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff, attempt, message)

type Result = { title :: String, url :: String, excerpt :: String }

type Search = String -> Aff (Either String (Array Result))

search :: String -> Search
search apiKey query = runExceptT do
  reply <- withExceptT message $ ExceptT $ attempt $ Http.post
    { url: "https://api.exa.ai/search"
    , headers: [ { name: "x-api-key", value: apiKey } ]
    , body: CA.encode request { query, numResults: 5, "type": "auto", contents: { text: { maxCharacters: 600 } } }
    }
  when (reply.status /= 200) $ throwError $ "Exa answered " <> show reply.status <> ": " <> J.stringify reply.body
  found <- except $ lmap CA.printJsonDecodeError $ CA.decode page reply.body
  pure $ found.results <#> \r ->
    { title: fromMaybe r.url (join r.title), url: r.url, excerpt: fromMaybe "" (join r.text) }

-- | The body Exa wants: a query, five results, up to 600 characters of page text.
request :: JsonCodec { query :: String, numResults :: Int, "type" :: String, contents :: { text :: { maxCharacters :: Int } } }
request = CAR.object "Search"
  { query: CA.string
  , numResults: CA.int
  , "type": CA.string
  , contents: CAR.object "Contents" { text: CAR.object "Text" { maxCharacters: CA.int } }
  }

-- | `title` and `text` may be missing or null.
page :: JsonCodec { results :: Array { title :: Maybe (Maybe String), url :: String, text :: Maybe (Maybe String) } }
page = CAR.object "Exa"
  { results: CA.array $ CAR.object "Result"
      { title: CAR.optional (Compat.maybe CA.string)
      , url: CA.string
      , text: CAR.optional (Compat.maybe CA.string)
      }
  }
