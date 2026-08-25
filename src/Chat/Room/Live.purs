module Chat.Room.Live
  ( assistantName
  , roomLive
  , roomLiveWith
  ) where

import Prelude

import Ai (Model, invoke, mount, text, tool)
import Ai.Model as Model
import Ai.DeepSeek as DeepSeek
import Ai.Schema as Schema
import Chat.Room (Message, PostError(..), RoomApi, RoomEvents, maxTextLength, room)
import Cloudflare.Durable (Live, Runtime)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Rpc (Rpc, fail)
import Cloudflare.Durable.Runtime (liftRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Durable.Storage as Storage
import Data.Array (last, nub, snoc, takeEnd)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains, joinWith, length, toLower, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (inj)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

messagesKey :: Storage.Key (Array Message)
messagesKey = Storage.key "messages"

-- | Message ids that mentioned the assistant and await a reply.
pendingKey :: Storage.Key (Array Int)
pendingKey = Storage.key "assistant.pending"

keptMessages :: Int
keptMessages = 500

assistantName :: String
assistantName = "ai"

mentionsAssistant :: String -> Boolean
mentionsAssistant = contains (Pattern ("@" <> assistantName)) <<< toLower

roomLive :: Live "Room" RoomApi RoomEvents
roomLive = roomLiveWith (DeepSeek.model <<< DeepSeek.flash)

-- | The room, given how to build a model from the API key. Tests pass a
-- | scripted one. A post that mentions `@ai` is queued and answered from the
-- | alarm, so the reply survives the request that asked for it.
roomLiveWith :: (String -> Model Aff) -> Live "Room" RoomApi RoomEvents
roomLiveWith modelFor =
  Durable.implementWith room ado
    state <- Durable.state
    sockets <- Durable.sockets room
    apiKey <- Durable.optional "DEEPSEEK_API_KEY"
    in
      do
        stored <- Storage.get state messagesKey
        messages <- liftEffect $ Ref.new $ fromMaybe [] stored
        let
          -- One channel per event: `cmap` narrows the socket to that case.
          posted = cmap (inj (Proxy :: Proxy "message")) sockets :: Sockets Message
          joined = cmap (inj (Proxy :: Proxy "joined")) sockets :: Sockets String
          left = cmap (inj (Proxy :: Proxy "left")) sockets :: Sockets String
          typing = cmap (inj (Proxy :: Proxy "typing")) sockets :: Sockets String

          record :: String -> String -> Runtime Message
          record author text = do
            sentAt <- liftEffect $ unwrap <<< unInstant <$> now
            all <- liftEffect $ Ref.read messages
            let message = { id: maybe 1 (\m -> m.id + 1) (last all), author, text, sentAt }
            let kept = takeEnd keptMessages $ snoc all message
            Storage.put state messagesKey kept
            liftEffect $ Ref.write kept messages
            Sockets.broadcast posted message
            pure message

          members :: forall e. Rpc e (Array String)
          members = nub <<< map _.tag <$> Sockets.connected sockets

          post :: _ -> Rpc PostError Message
          post new = do
            let author = trim new.author
            let text = trim new.text
            when (author == "") $ fail AuthorRequired
            when (text == "") $ fail TextRequired
            when (length text > maxTextLength) $ fail TextTooLong
            message <- liftRuntime $ record author text
            when (mentionsAssistant text && author /= assistantName) do
              queued <- fromMaybe [] <$> Storage.get state pendingKey
              Storage.put state pendingKey (snoc queued message.id)
              Alarm.scheduleIn state (Milliseconds 0.0)
            pure message

          -- The assistant, as an agent over the room: recent messages are its
          -- prompt, the room's own capabilities are its tools.
          answer :: Runtime Unit
          answer = do
            pending <- fromMaybe [] <$> Storage.get state pendingKey
            unless (pending == []) do
              void $ Storage.delete state pendingKey
              Sockets.broadcast typing assistantName
              recent <- takeEnd 20 <$> liftEffect (Ref.read messages)
              let
                whoIsHere = tool "members" "Who is in the room right now" (Schema.object {}) (Schema.array Schema.string)
                  \_ -> nub <<< map _.tag <$> Sockets.connected sockets
                transcript = joinWith "\n" $ recent <#> \m -> m.author <> ": " <> m.text
                persona = text $ "You are '" <> assistantName <> "', a member of a small chat room. Reply in one or two short sentences, "
                  <> "as yourself, to whoever mentioned you last. Do not prefix your name."
              reply <- case apiKey of
                Nothing -> pure $ Left $ Model.Misconfigured "DEEPSEEK_API_KEY is not set"
                Just key -> invoke (mount (Model.hoist liftAff (modelFor key)) [ whoIsHere ] persona) transcript
              void $ record assistantName case reply of
                Right said -> said
                Left failure -> "(I could not answer: " <> show failure <> ")"

        pure $ Durable.handlers
          { post
          , history: \_ -> liftEffect $ Ref.read messages
          , members: \_ -> members
          , typing: Sockets.broadcast typing
          }
          # _
            { alarm = answer
            , connect = \socket -> Sockets.broadcast joined socket.tag
            , disconnect = \socket -> Sockets.broadcast left socket.tag
            }
