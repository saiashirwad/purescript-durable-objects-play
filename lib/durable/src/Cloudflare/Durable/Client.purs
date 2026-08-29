-- | The browser end of the Durable Object transport.
module Cloudflare.Durable.Client
  ( connect
  ) where

import Prelude

import Cloudflare.Durable.Core (Id(..), Listener, Namespace, Object, className, namespace)
import Cloudflare.Durable.Events (Signal(..))
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Fetch (Method(..), fetch)
import Foreign (readString, renderForeignError)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as SocketEvent
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Socket.WebSocket as WebSocket

-- | The browser end: the same namespace, reached over HTTP under `prefix`.
connect :: forall name api events. String -> Object name api events -> Namespace name api events
connect prefix object = namespace object
  { call: \id methodName body -> postJson (url id methodName) body
  , listen: \id tag (deliver :: Listener) ->
      openSocket (url id "socket?tag=" <> tag) (deliver Opened) (deliver Closed) (deliver <<< Delivered) (deliver <<< Garbled)
  , fetch: \_ _ -> throwError $ error "fetch into an object is for Workers; a browser goes through Http.route"
  , unique: do
      response <- postJson (base <> "/new") J.jsonNull
      case CA.decode idCodec response of
        Right { id } -> pure $ Unique id
        Left err -> throwError $ error $ "Cloudflare.Durable.Client: bad id response: " <> CA.printJsonDecodeError err
  }
  where
  base = prefix <> "/" <> className object
  url id methodName = base <> "/" <> idSegments id <> "/" <> methodName

postJson :: String -> Json -> Aff Json
postJson url body = do
  response <- fetch url { method: POST, headers: { "content-type": "application/json" }, body: J.stringify body }
  unless response.ok $ throwError $ error $ "HTTP " <> show response.status <> " from " <> url
  text <- response.text
  either (throwError <<< error) pure (jsonParser text)

-- | Reconnects two seconds after an unexpected close; the returned closer stops that.
openSocket :: String -> Effect Unit -> Effect Unit -> (Json -> Effect Unit) -> (String -> Effect Unit) -> Effect (Effect Unit)
openSocket path onOpen onClose onMessage onGarbled = do
  url <- socketUrl path
  stopped <- Ref.new false
  timer <- Ref.new Nothing
  current <- Ref.new Nothing
  opened <- eventListener \_ -> onOpen
  received <- eventListener \event -> for_ (MessageEvent.fromEvent event) \message ->
    either onGarbled onMessage $ decode (MessageEvent.data_ message)
  let
    open = do
      socket <- WebSocket.create url []
      Ref.write (Just socket) current
      closed <- eventListener \_ -> do
        onClose
        unlessM (Ref.read stopped) $ setTimeout 2000 open >>= Just >>> flip Ref.write timer
      let target = WebSocket.toEventTarget socket
      addEventListener SocketEvent.onOpen opened false target
      addEventListener SocketEvent.onMessage received false target
      addEventListener SocketEvent.onClose closed false target
  open
  pure do
    Ref.write true stopped
    Ref.read timer >>= traverse_ clearTimeout
    Ref.read current >>= traverse_ WebSocket.close
  where
  decode payload = do
    text <- lmap (foldMap renderForeignError) $ runExcept $ readString payload
    jsonParser text

-- | A path becomes a socket URL on this origin; a full URL is left alone.
socketUrl :: String -> Effect String
socketUrl path = case stripPrefix (Pattern "/") path of
  Nothing -> pure path
  Just _ -> do
    location <- Window.location =<< window
    protocol <- Location.protocol location
    host <- Location.host location
    pure $ (if protocol == "https:" then "wss://" else "ws://") <> host <> path

idSegments :: Id -> String
idSegments = case _ of
  Named name -> "name/" <> name
  Unique id -> "id/" <> id

idCodec :: JsonCodec { id :: String }
idCodec = CAR.object "Id" { id: CA.string }
