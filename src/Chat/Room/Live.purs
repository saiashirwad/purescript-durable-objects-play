module Chat.Room.Live
  ( assistantName
  , roomLive
  , roomLiveWith
  ) where

import Prelude

import Ai (Model, invoke, mount, text, tool)
import Control.Alt ((<|>))
import Ai.DeepSeek as DeepSeek
import Ai.Model as Model
import Ai.Schema as Schema
import Chat.Markdown as Markdown
import Chat.Room (Message, NewMessage, PostError(..), ReactError(..), RoomApi, RoomEvents, maxTextLength, room)
import Cloudflare.Durable (Live, Runtime, State)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Rpc (Rpc, fail)
import Cloudflare.Durable.Runtime (liftRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Durable.Sql (Statement)
import Cloudflare.Durable.Sql as Sql
import Cloudflare.Durable.Storage as Storage
import Cloudflare.Worker as Worker
import Data.Array (any, elem, filter, find, last, nub, snoc, takeEnd)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime.Instant (unInstant)
import Data.Divide (divided)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Functor.Contravariant (cmap)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), contains, joinWith, length, stripPrefix, toLower, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (inj)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

-- Storage ------------------------------------------------------------------

messagesKey :: Storage.Key (Array Message)
messagesKey = Storage.key "messages.v2"

-- | The shape before replies, images and reactions; read once and upgraded.
type Legacy = { id :: Int, author :: String, text :: String, sentAt :: Number }

legacyKey :: Storage.Key (Array Legacy)
legacyKey = Storage.key "messages"

upgrade :: Legacy -> Message
upgrade m = { id: m.id, author: m.author, text: m.text, images: [], replyTo: Nothing, mentions: Markdown.mentions m.text, reactions: [], sentAt: m.sentAt }

pendingKey :: Storage.Key (Array Int)
pendingKey = Storage.key "assistant.pending"

keptMessages :: Int
keptMessages = 500

-- Images, in the room's SQLite, served by the fetch hook ---------------------

createImages :: Statement Unit Unit
createImages = Sql.statement
  "CREATE TABLE IF NOT EXISTS images (id INTEGER PRIMARY KEY, mime TEXT NOT NULL, data TEXT NOT NULL)"
  Sql.noParams
  (pure unit)

insertImage :: Statement { mime :: String, data :: String } Int
insertImage = lcmap (\i -> i.mime /\ i.data) $ Sql.statement
  "INSERT INTO images (mime, data) VALUES (?, ?) RETURNING id"
  (Sql.param CA.string `divided` Sql.param CA.string)
  (Sql.columnOf "id")

selectImage :: Statement Int { mime :: String, data :: String }
selectImage = Sql.statement
  "SELECT mime, data FROM images WHERE id = ?"
  Sql.paramOf
  ({ mime: _, data: _ } <$> Sql.columnOf "mime" <*> Sql.columnOf "data")

imageExists :: Statement Int Int
imageExists = Sql.statement "SELECT id FROM images WHERE id = ?" Sql.paramOf (Sql.columnOf "id")

-- | Base64 of 4 MB.
maxImageChars :: Int
maxImageChars = 5600000

-- | `POST /image` (body: the bytes, header: its type) answers `{ "id": n }`;
-- | `GET /image/<n>` serves it.
images :: State -> Worker.Request -> Runtime Worker.Response
images state request = case Worker.method request, Worker.pathname request of
  "POST", "/image" -> do
    let mime = fromMaybe "" $ Worker.header request "content-type"
    if stripPrefix (Pattern "image/") mime == Nothing then pure $ Worker.text 415 "send an image/* body"
    else do
      body <- liftAff $ Worker.bodyBase64 request
      if length body > maxImageChars then pure $ Worker.text 413 "image too large"
      else do
        id <- fromMaybe 0 <$> Sql.first state insertImage { mime, data: body }
        pure $ Worker.json 200 $ CA.encode (CAR.object "Image" { id: CA.int }) { id }
  "GET", path | Just id <- stripPrefix (Pattern "/image/") path >>= fromString -> do
    found <- Sql.first state selectImage id
    pure case found of
      Just image -> Worker.bytes 200 image.mime image.data
      Nothing -> Worker.text 404 "no such image"
  _, _ -> pure $ Worker.text 404 "not found"

-- Assistant ----------------------------------------------------------------

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
        Sql.execute state createImages unit
        stored <- Storage.get state messagesKey
        legacy <- case stored of
          Just _ -> pure Nothing
          Nothing -> map (map upgrade) <$> Storage.get state legacyKey
        messages <- liftEffect $ Ref.new $ fromMaybe [] $ stored <|> legacy
        let
          -- One channel per event: `cmap` narrows the socket to that case.
          posted = cmap (inj (Proxy :: Proxy "message")) sockets :: Sockets Message
          updated = cmap (inj (Proxy :: Proxy "updated")) sockets :: Sockets Message
          joined = cmap (inj (Proxy :: Proxy "joined")) sockets :: Sockets String
          left = cmap (inj (Proxy :: Proxy "left")) sockets :: Sockets String
          typing = cmap (inj (Proxy :: Proxy "typing")) sockets :: Sockets String

          save :: Array Message -> Runtime Unit
          save all = do
            Storage.put state messagesKey all
            liftEffect $ Ref.write all messages

          record :: NewMessage -> Runtime Message
          record new = do
            sentAt <- liftEffect $ unwrap <<< unInstant <$> now
            all <- liftEffect $ Ref.read messages
            let
              message =
                { id: maybe 1 (\m -> m.id + 1) (last all)
                , author: new.author
                , text: new.text
                , images: new.images
                , replyTo: new.replyTo
                , mentions: Markdown.mentions new.text
                , reactions: []
                , sentAt
                }
            save $ takeEnd keptMessages $ snoc all message
            Sockets.broadcast posted message
            pure message

          members :: forall e. Rpc e (Array String)
          members = nub <<< map _.tag <$> Sockets.connected sockets

          post :: NewMessage -> Rpc PostError Message
          post new = do
            let author = trim new.author
            let body = trim new.text
            when (author == "") $ fail AuthorRequired
            when (body == "" && new.images == []) $ fail TextRequired
            when (length body > maxTextLength) $ fail TextTooLong
            all <- liftEffect $ Ref.read messages
            for_ new.replyTo \id -> unless (any (\m -> m.id == id) all) $ fail $ NoSuchReply id
            for_ new.images \id -> do
              found <- Sql.first state imageExists id
              unless (isJust found) $ fail $ NoSuchImage id
            message <- liftRuntime $ record new { author = author, text = body }
            when (mentionsAssistant body && author /= assistantName) do
              queued <- fromMaybe [] <$> Storage.get state pendingKey
              Storage.put state pendingKey (snoc queued message.id)
              Alarm.scheduleIn state (Milliseconds 0.0)
            pure message

          -- Toggle `by` on `emoji`; a reaction nobody holds disappears.
          react :: { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
          react { id, emoji, by } = do
            when (trim emoji == "") $ fail EmojiRequired
            all <- liftEffect $ Ref.read messages
            case find (\m -> m.id == id) all of
              Nothing -> fail $ NoSuchMessage id
              Just message -> do
                let
                  toggled = case find (\r -> r.emoji == emoji) message.reactions of
                    Nothing -> snoc message.reactions { emoji, by: [ by ] }
                    Just r ->
                      let by' = if by `elem` r.by then filter (_ /= by) r.by else snoc r.by by
                      in
                        filter (\x -> x.by /= []) $ message.reactions <#> \x -> if x.emoji == emoji then x { by = by' } else x
                  message' = message { reactions = toggled }
                liftRuntime do
                  save $ all <#> \m -> if m.id == id then message' else m
                  Sockets.broadcast updated message'
                pure message'

          -- The assistant, as an agent over the room: recent messages are its
          -- prompt, the room's own capabilities are its tools. It replies in
          -- the thread of the message that mentioned it.
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
                  <> "as yourself, to whoever mentioned you last. Markdown is fine. Do not prefix your name."
              reply <- case apiKey of
                Nothing -> pure $ Left $ Model.Misconfigured "DEEPSEEK_API_KEY is not set"
                Just key -> invoke (mount (Model.hoist liftAff (modelFor key)) [ whoIsHere ] persona) transcript
              void $ record
                { author: assistantName
                , images: []
                , replyTo: last pending
                , text: case reply of
                    Right said -> said
                    Left failure -> "(I could not answer: " <> show failure <> ")"
                }

        pure $ Durable.handlers
          { post
          , react
          , history: \_ -> liftEffect $ Ref.read messages
          , members: \_ -> members
          , typing: Sockets.broadcast typing
          }
          # _
            { alarm = answer
            , fetch = images state
            , connect = \socket -> Sockets.broadcast joined socket.tag
            , disconnect = \socket -> Sockets.broadcast left socket.tag
            }
