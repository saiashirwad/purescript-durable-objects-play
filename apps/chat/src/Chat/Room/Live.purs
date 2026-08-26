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
import Chat.Room (Message, NewMessage, PostError(..), ReactError(..), Reaction, RoomApi, RoomEvents, assistantName, maxTextLength, room)
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
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (any, elem, filter, find, last, nub, null, snoc, takeEnd)
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
import Data.String (Pattern(..), contains, joinWith, length, stripPrefix, toLower, trim)
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

-- | Plan what the room needs, then load it. The history comes from the
-- | first source that has one: `<|>` on `MaybeT` stops at the first `Just`.
open :: (String -> Model Aff) -> Init (Runtime Room)
open modelFor = ado
  state <- Durable.state
  sockets <- Durable.sockets room
  apiKey <- Durable.optional "DEEPSEEK_API_KEY"
  exaKey <- Durable.optional "EXA_API_KEY"
  in
    do
      Sql.execute state createImages unit
      history <- runMaybeT $ MaybeT (Storage.get state messagesKey) <|> map upgrade <$> MaybeT (Storage.get state legacyKey)
      messages <- liftEffect $ Ref.new $ fromMaybe [] history
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

pendingKey :: Storage.Key (Array Int)
pendingKey = Storage.key "assistant.pending"

keptMessages :: Int
keptMessages = 500

save :: Room -> Array Message -> Runtime Unit
save r all = do
  Storage.put r.state messagesKey all
  liftEffect $ Ref.write all r.messages

-- Messages ----------------------------------------------------------------------

members :: forall m. MonadRuntime m => Room -> m (Array String)
members r = nub <<< map _.tag <$> Sockets.connected r.emit.all

-- | Number, stamp, store and broadcast a message.
record :: Room -> NewMessage -> Runtime Message
record r new = do
  sentAt <- liftEffect $ unwrap <<< unInstant <$> now
  all <- liftEffect $ Ref.read r.messages
  let
    message =
      { id: maybe 1 (_.id >>> (_ + 1)) (last all)
      , author: new.author
      , text: new.text
      , images: new.images
      , replyTo: new.replyTo
      , mentions: Markdown.mentions new.text
      , reactions: []
      , sentAt
      }
  save r $ takeEnd keptMessages $ snoc all message
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
  when (mentionsAssistant body && author /= assistantName) do
    queued <- fromMaybe [] <$> Storage.get r.state pendingKey
    Storage.put r.state pendingKey (snoc queued message.id)
    Alarm.scheduleIn r.state (Milliseconds 0.0)
  pure message

react :: Room -> { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
react r { id, emoji, by } = do
  when (trim emoji == "") $ fail EmojiRequired
  all <- liftEffect $ Ref.read r.messages
  case find (_.id >>> eq id) all of
    Nothing -> fail $ NoSuchMessage id
    Just found -> do
      let message = found { reactions = toggle emoji by found.reactions }
      liftRuntime do
        save r $ all <#> \m -> if m.id == id then message else m
        Sockets.broadcast r.emit.updated message
      pure message

-- | Flip `by` on `emoji`; a reaction nobody holds disappears.
toggle :: String -> String -> Array Reaction -> Array Reaction
toggle emoji by reactions = case find (_.emoji >>> eq emoji) reactions of
  Nothing -> snoc reactions { emoji, by: [ by ] }
  Just _ -> filter (not <<< null <<< _.by) $ reactions <#> \x ->
    if x.emoji /= emoji then x
    else x { by = if by `elem` x.by then filter (_ /= by) x.by else snoc x.by by }

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
        else do
          id <- fromMaybe 0 <$> Sql.first state insertImage { mime: fromMaybe "" (Worker.header request "content-type"), data: body }
          pure $ Just $ Worker.json 200 $ CA.encode (CAR.object "Image" { id: CA.int }) { id }
    | otherwise -> pure $ Just $ Worker.text 415 "send an image/* body"
  "GET", path | Just id <- stripPrefix (Pattern "/image/") path >>= fromString ->
    Sql.first state selectImage id <#> case _ of
      Just image -> Just $ Worker.bytes 200 image.mime image.data
      Nothing -> Just $ Worker.text 404 "no such image"
  _, _ -> pure Nothing

-- The assistant --------------------------------------------------------------------

mentionsAssistant :: String -> Boolean
mentionsAssistant = contains (Pattern ("@" <> assistantName)) <<< toLower

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
  pending <- fromMaybe [] <$> Storage.get r.state pendingKey
  unless (null pending) do
    void $ Storage.delete r.state pendingKey
    Sockets.broadcast r.emit.typing assistantName
    transcript <- recap <<< takeEnd 20 <$> liftEffect (Ref.read r.messages)
    reply <- maybe (pure $ Left $ Model.Misconfigured "DEEPSEEK_API_KEY is not set") (_ `invoke` transcript) (agentFor r)
    void $ record r
      { author: assistantName
      , images: []
      , replyTo: last pending
      , text: either (\failure -> "(I could not answer: " <> show failure <> ")") identity reply
      }
  where
  recap = joinWith "\n" <<< map \m -> m.author <> ": " <> m.text
