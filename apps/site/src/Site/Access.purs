-- | One shared passkey. `POST /login { passkey }` sets a session cookie
-- | holding a hash of it; `access` admits requests that carry it. Fetch and
-- | WebSocket both send cookies, so nothing else changes.
module Site.Access
  ( access
  , login
  ) where

import Prelude

import Cloudflare.Worker (Request, Route, Worker, WorkerInit)
import Cloudflare.Worker as Worker
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Effect.Aff (Aff)

cookieName :: String
cookieName = "session"

token :: String -> Aff String
token passkey = Worker.sha256 $ "session:" <> passkey

-- | True when the request's cookie matches the configured passkey.
access :: WorkerInit (Request -> Aff Boolean)
access = Worker.variable "PASSKEY" <#> \passkey request -> do
  expected <- token passkey
  pure $ Worker.cookie request cookieName == Just expected

login :: Worker
login = Worker.make $ Worker.variable "PASSKEY" <#> \passkey -> loginRoute passkey <> sessionRoute passkey

loginRoute :: String -> Route
loginRoute passkey = Worker.route \request ->
  case Worker.method request, Worker.pathname request of
    "POST", "/login" -> do
      body <- Worker.body request
      case CA.decode (CAR.object "Login" { passkey: CA.string }) body of
        Right { passkey: offered } | offered == passkey -> do
          expected <- token passkey
          let secure = if stripPrefix (Pattern "https:") (Worker.url request) == Nothing then "" else "; Secure"
          pure $ Just $ Worker.textWith 204
            [ { name: "set-cookie", value: cookieName <> "=" <> expected <> "; Path=/; HttpOnly; SameSite=Lax; Max-Age=2592000" <> secure } ]
            ""
        _ -> pure $ Just $ Worker.text 401 "wrong passkey"
    _, _ -> pure Nothing

-- | `GET /session`: 204 when the cookie is good, else 401.
sessionRoute :: String -> Route
sessionRoute passkey = Worker.route \request ->
  case Worker.method request, Worker.pathname request of
    "GET", "/session" -> do
      expected <- token passkey
      pure $ Just if Worker.cookie request cookieName == Just expected then Worker.text 204 "" else Worker.text 401 "passkey required"
    _, _ -> pure Nothing
