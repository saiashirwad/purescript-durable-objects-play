module Chat.Room.Live
  ( roomLive
  , roomLiveWith
  ) where

import Prelude

import Ai (Agent, Def, Model, invoke, mount, text, tool)
import Ai.Catalogue as Catalogue
import Ai.Exa as Exa
import Ai.Model as Model
import Ai.Provider as Provider
import Ai.Schema as Schema
import Markdown as Markdown
import Chat.Room (Message, NewMessage, PostError(..), ReactError(..), RoomApi, RoomEvents, appendMessage, assistantName, maxTextLength, room, toggleReaction)
import Cloudflare.Durable (Handlers, Init, Live, Runtime, State)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Rpc (Rpc, fail)
import Cloudflare.Durable.Runtime (class MonadRuntime, liftRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Durable.Sql (Statement)
import Cloudflare.Durable.Sql as Sql
import Cloudflare.Durable.Storage as Storage
import Cloudflare.Worker as Worker
import Data.Array (any, find, last, nub, null, takeEnd)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime.Instant (unInstant)
import Data.Divide (divided)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, for_)
import Data.Functor.Contravariant (cmap)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), joinWith, length, stripPrefix, toLower, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

-- The room -------------------------------------------------------------------

-- | Everything a handler needs, built once per activation.
type Room =
  { state :: State
  , messages :: Ref (Array Message)
  , emit :: Channels
  , assistant :: Maybe (Model Aff)
  , search :: Maybe Exa.Search
  }

-- | The socket, narrowed to each event: `cmap` on a `Contravariant`.
type Channels =
  { all :: Sockets (Variant RoomEvents)
  , message :: Sockets Message
  , updated :: Sockets Message
  , joined :: Sockets String
  , left :: Sockets String
  , typing :: Sockets String
  }

channels :: Sockets (Variant RoomEvents) -> Channels
channels all =
  { all
  , message: cmap (inj (Proxy :: Proxy "message")) all
  , updated: cmap (inj (Proxy :: Proxy "updated")) all
  , joined: cmap (inj (Proxy :: Proxy "joined")) all
  , left: cmap (inj (Proxy :: Proxy "left")) all
  , typing: cmap (inj (Proxy :: Proxy "typing")) all
  }

roomLive :: Live "Room" RoomApi RoomEvents
roomLive = roomLiveWith \key -> Provider.model Provider.deepseek key Catalogue.deepseekFlash

-- | The room, given how to build a model from the API key. Tests pass a
-- | scripted one.
roomLiveWith :: (String -> Model Aff) -> Live "Room" RoomApi RoomEvents
roomLiveWith modelFor = Durable.implementWith room $ map handlersFor <$> open modelFor

-- | Plan what the room needs, then load it.
open :: (String -> Model Aff) -> Init (Runtime Room)
open modelFor = ado
  state <- Durable.state
  sockets <- Durable.sockets room
  apiKey <- Durable.optional "DEEPSEEK_API_KEY"
  exaKey <- Durable.optional "EXA_API_KEY"
  in
    do
      Sql.execute state createImages unit
      history <- loadMessages state
      messages <- liftEffect $ Ref.new history
      pure { state, messages, emit: channels sockets, assistant: modelFor <$> apiKey, search: Exa.search <$> exaKey }

handlersFor :: Room -> Handlers RoomApi
handlersFor r =
  Durable.handlers
    { post: post r
    , react: react r
    , history: \_ -> liftEffect $ Ref.read r.messages
    , members: \_ -> members r
    , typing: Sockets.broadcast r.emit.typing
    }
    `Durable.withHooks`
      ( Durable.alarmHook (answer r)
          <> Durable.fetchHook (images r.state)
          <> Durable.connectHook (Sockets.broadcast r.emit.joined <<< _.tag)
          <> Durable.disconnectHook (Sockets.broadcast r.emit.left <<< _.tag)
      )

-- Storage ---------------------------------------------------------------------

messagesKey :: Storage.Key (Array Message)
messagesKey = Storage.key "messages.v2"

-- | The shape before replies, images and reactions; read once and upgraded.
type Legacy = { id :: Int, author :: String, text :: String, sentAt :: Number }

legacyKey :: Storage.Key (Array Legacy)
legacyKey = Storage.key "messages"

upgrade :: Legacy -> Message
upgrade m =
  { id: m.id, author: m.author, text: m.text, images: [], replyTo: Nothing, mentions: Markdown.mentions m.text, reactions: [], sentAt: m.sentAt }

pendingKey :: Storage.Key Int
pendingKey = Storage.key "assistant.pending.v2"

legacyPendingKey :: Storage.Key (Array Int)
legacyPendingKey = Storage.key "assistant.pending"

-- | Prefer v2 history. Persist a legacy upgrade but keep the legacy data.
loadMessages :: State -> Runtime (Array Message)
loadMessages state = Storage.get state messagesKey >>= case _ of
  Just messages -> pure messages
  Nothing -> Storage.get state legacyKey >>= case _ of
    Nothing -> pure []
    Just legacy -> do
      let messages = upgrade <$> legacy
      Storage.put state messagesKey messages
      pure messages

loadPending :: State -> Runtime (Maybe Int)
loadPending state = Storage.get state pendingKey >>= case _ of
  Just id -> pure $ Just id
  Nothing -> last <<< fromMaybe [] <$> Storage.get state legacyPendingKey

save :: Room -> Array Message -> Runtime Unit
save r all = do
  Storage.put r.state messagesKey all
  liftEffect $ Ref.write all r.messages

-- Messages ----------------------------------------------------------------------

members :: forall m. MonadRuntime m => Room -> m (Array String)
members r = nub <<< map _.tag <$> Sockets.connected r.emit.all

-- | Stamp, interpret the pure append transition, store, and broadcast.
record :: Room -> NewMessage -> Runtime Message
record r new = do
  sentAt <- liftEffect $ unwrap <<< unInstant <$> now
  all <- liftEffect $ Ref.read r.messages
  let message /\ messages = appendMessage sentAt new all
  save r messages
  Sockets.broadcast r.emit.message message
  pure message

-- | Validate, record, and queue a reply if the assistant was mentioned.
post :: Room -> NewMessage -> Rpc PostError Message
post r new = do
  let author = trim new.author
  let body = trim new.text
  when (author == "") $ fail AuthorRequired
  when (body == "" && null new.images) $ fail TextRequired
  when (length body > maxTextLength) $ fail TextTooLong
  all <- liftEffect $ Ref.read r.messages
  for_ new.replyTo \id -> unless (any (_.id >>> eq id) all) $ fail $ NoSuchReply id
  for_ new.images \id -> do
    found <- Sql.first r.state imageExists id
    unless (isJust found) $ fail $ NoSuchImage id
  message <- liftRuntime $ record r new { author = author, text = body }
  when (toLower author /= toLower assistantName && any (\mention -> toLower mention == toLower assistantName) message.mentions) do
    Storage.put r.state pendingKey message.id
    Alarm.scheduleIn r.state (Milliseconds 0.0)
  pure message

react :: Room -> { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
react r { id, emoji, by } = do
  let normalizedEmoji = trim emoji
  let normalizedReactor = trim by
  when (normalizedEmoji == "") $ fail EmojiRequired
  when (normalizedReactor == "") $ fail ReactorRequired
  all <- liftEffect $ Ref.read r.messages
  case find (_.id >>> eq id) all of
    Nothing -> fail $ NoSuchMessage id
    Just found -> do
      let message = found { reactions = toggleReaction normalizedEmoji normalizedReactor found.reactions }
      liftRuntime do
        save r $ all <#> \m -> if m.id == id then message else m
        Sockets.broadcast r.emit.updated message
      pure message

-- Images, in the room's SQLite, served by the fetch hook -------------------------

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
-- | `GET /image/<n>` serves it. A request that does not match an image route
-- | returns `Nothing`, so other fetch hooks get their turn.
images :: State -> Worker.Request -> Runtime (Maybe Worker.Response)
images state request = case Worker.method request, Worker.pathname request of
  "POST", "/image"
    | Just _ <- Worker.header request "content-type" >>= stripPrefix (Pattern "image/") -> do
        body <- liftAff $ Worker.bodyBase64 request
        if length body > maxImageChars then pure $ Just $ Worker.text 413 "image too large"
        else Sql.first state insertImage { mime: fromMaybe "" (Worker.header request "content-type"), data: body } <#> case _ of
          Nothing -> Just $ Worker.text 500 "could not store image"
          Just id -> Just $ Worker.json 200 $ CA.encode (CAR.object "Image" { id: CA.int }) { id }
    | otherwise -> pure $ Just $ Worker.text 415 "send an image/* body"
  "GET", path | Just id <- stripPrefix (Pattern "/image/") path >>= fromString ->
    Sql.first state selectImage id <#> case _ of
      Just image -> Just $ Worker.bytes 200 image.mime image.data
      Nothing -> Just $ Worker.text 404 "no such image"
  _, _ -> pure Nothing

-- The assistant --------------------------------------------------------------------

persona :: Def String String
persona = text $
  "You are '" <> assistantName <> "', a member of a small chat room. Reply in one or two short sentences, "
    <> "as yourself, to whoever mentioned you last. Markdown is fine. Do not prefix your name. "
    <> "Use the search tool for anything recent or anything you are not sure of, and link the page you relied on."

-- | The assistant as an agent over the room: the room's own capabilities and
-- | the web are its tools, and the model (in `Aff`) is hoisted into `Runtime`.
agentFor :: Room -> Maybe (Agent Runtime String String)
agentFor r = r.assistant <#> \model -> mount (Model.hoist liftAff model) ([ whoIsHere ] <> foldMap (pure <<< web) r.search) persona
  where
  whoIsHere = tool "members" "Who is in the room right now" (Schema.object {}) (Schema.array Schema.string) \_ -> members r
  web search = tool "search" "Search the web; returns pages with a short excerpt of each"
    (Schema.object { query: Schema.describe "What to look for, as you would type it into a search engine" Schema.string })
    (Schema.object { results: Schema.array result, error: Schema.nullable Schema.string })
    \{ query } -> liftAff (search query) <#> either (\why -> { results: [], error: Just why }) (\results -> { results, error: Nothing })
  result = Schema.object { title: Schema.string, url: Schema.string, excerpt: Schema.string }

-- | Runs from the alarm, so the reply survives the request that asked for
-- | it. Recent messages are the prompt; the reply threads under the last
-- | message that mentioned the assistant.
answer :: Room -> Runtime Unit
answer r = do
  pending <- loadPending r.state
  for_ pending \id -> do
    void $ Storage.delete r.state pendingKey
    void $ Storage.delete r.state legacyPendingKey
    Sockets.broadcast r.emit.typing assistantName
    transcript <- recap <<< takeEnd 20 <$> liftEffect (Ref.read r.messages)
    reply <- maybe (pure $ Left $ Model.Misconfigured "DEEPSEEK_API_KEY is not set") (_ `invoke` transcript) (agentFor r)
    void $ record r
      { author: assistantName
      , images: []
      , replyTo: Just id
      , text: either (\failure -> "(I could not answer: " <> show failure <> ")") identity reply
      }
  where
  recap = joinWith "\n" <<< map \m -> m.author <> ": " <> m.text
