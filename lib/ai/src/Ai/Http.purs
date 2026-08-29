-- | The one way an `Ai` provider touches the network. Providers differ only
-- | in the `url` and `headers` they hand this.
module Ai.Http
  ( Header
  , Post
  , post
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Either (fromRight)
import Data.HTTP.Method (Method(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import JS.Fetch as Fetch
import JS.Fetch.Headers as Headers
import JS.Fetch.Request as Request
import JS.Fetch.RequestBody as Body
import JS.Fetch.Response as Response
import Promise.Aff (toAffE)

type Header = { name :: String, value :: String }

type Post = { url :: String, headers :: Array Header, body :: Json } -> Aff { status :: Int, body :: Json }

-- | Runs on Node 18+ and in Workers. A body that is not JSON comes back as a string.
post :: Post
post { url, headers, body } = do
  request <- liftEffect $ Request.new url
    { method: POST
    , headers: Headers.fromFoldable $ [ "content-type" /\ "application/json" ] <> (headers <#> \{ name, value } -> name /\ value)
    , body: Body.fromString $ J.stringify body
    }
  response <- toAffE $ Fetch.fetch request
  text <- toAffE $ Response.text response
  pure { status: Response.status response, body: fromRight (J.fromString text) (jsonParser text) }
