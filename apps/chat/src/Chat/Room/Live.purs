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
import Chat.Room (Message, NewMessage, PostError(..), ReactError(..), RoomApi, RoomEvents, Snapshot, UserNameError(..), assistantName, maxTextLength, mkUserName, printUserName, room)
import Chat.Room.Images as Images
import Chat.Room.Store as Store
import Cloudflare.Durable (Handlers, Init, Live, Runtime, State)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Rpc (Rpc, fail)
import Cloudflare.Durable.Runtime (class MonadRuntime, liftRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Durable.Storage as Storage
import Data.Array (any, last, nub, null, takeEnd)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, for_)
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith, length, toLower, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Type.Proxy (Proxy(..))

-- The room -------------------------------------------------------------------

-- | Everything a handler needs, built once per activation.
type Room =
  { state :: State
  , store :: Store.Store
  , images :: Images.Images
  , emit :: Channels
  , assistant :: Maybe (Model Aff)
  , search :: Maybe Exa.Search
  }

-- | The socket, narrowed to each event: `cmap` on a `Contravariant`.
type Channels =
  { all :: Sockets (Variant RoomEvents)
  , message :: Sockets Message
  , updated :: Sockets Message
  , presence :: Sockets (Array String)
  , typing :: Sockets String
  }

channels :: Sockets (Variant RoomEvents) -> Channels
channels all =
  { all
  , message: cmap (inj (Proxy :: Proxy "message")) all
  , updated: cmap (inj (Proxy :: Proxy "updated")) all
  , presence: cmap (inj (Proxy :: Proxy "presence")) all
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
      images <- Images.open state
      store <- Store.open state
      history <- Store.snapshot store
      attachedAt <- unwrap <<< unInstant <$> Alarm.now state
      for_ history \message -> Images.attach images attachedAt message.images
      Images.cleanup images
      pure { state, store, images, emit: channels sockets, assistant: modelFor <$> apiKey, search: Exa.search <$> exaKey }

handlersFor :: Room -> Handlers RoomApi
handlersFor r =
  Durable.handlers
    { post: post r
    , react: react r
    , snapshot: snapshot r
    , typing: Sockets.broadcast r.emit.typing
    }
    `Durable.withHooks`
      ( Durable.alarmHook (answer r)
          <> Images.hooks r.images
          <> Durable.connectHook (const $ broadcastPresence r)
          <> Durable.disconnectHook (const $ broadcastPresence r)
      )

-- Storage ---------------------------------------------------------------------

pendingKey :: Storage.Key Int
pendingKey = Storage.key "assistant.pending.v2"

legacyPendingKey :: Storage.Key (Array Int)
legacyPendingKey = Storage.key "assistant.pending"

loadPending :: State -> Runtime (Maybe Int)
loadPending state = Storage.get state pendingKey >>= case _ of
  Just id -> pure $ Just id
  Nothing -> last <<< fromMaybe [] <$> Storage.get state legacyPendingKey

-- Messages ----------------------------------------------------------------------

members :: forall m. MonadRuntime m => Room -> m (Array String)
members r = nub <<< map _.tag <$> Sockets.connected r.emit.all

-- | Store one message and broadcast it after every write succeeds.
record :: Room -> NewMessage -> Runtime Message
record r new = do
  message <- Store.post r.store new
  Images.attach r.images message.sentAt new.images
  Sockets.broadcast r.emit.message message
  pure message

snapshot :: Room -> Unit -> Rpc Void Snapshot
snapshot r _ = do
  messages <- liftRuntime $ Store.snapshot r.store
  presence <- members r
  pure { messages, presence }

broadcastPresence :: Room -> Runtime Unit
broadcastPresence r = members r >>= Sockets.broadcast r.emit.presence

-- | Validate, record, and queue a reply if the assistant was mentioned.
post :: Room -> NewMessage -> Rpc PostError Message
post r new = do
  let body = trim new.text
  user <- case mkUserName new.author of
    Left UserNameRequired -> fail AuthorRequired
    Left UserNameTooLong -> fail AuthorTooLong
    Left UserNameInvalid -> fail AuthorInvalid
    Left UserNameReserved -> fail AuthorReserved
    Right accepted -> pure accepted
  let author = printUserName user
  when (body == "" && null new.images) $ fail TextRequired
  when (length body > maxTextLength) $ fail TextTooLong
  for_ new.replyTo \id -> do
    found <- liftRuntime $ Store.hasMessage r.store id
    unless found $ fail $ NoSuchReply id
  for_ new.images \id -> do
    found <- liftRuntime $ Images.exists r.images id
    unless found $ fail $ NoSuchImage id
  message <- liftRuntime $ record r new { author = author, text = body }
  when (toLower author /= toLower assistantName && any (\mention -> toLower mention == toLower assistantName) message.mentions) do
    Storage.put r.state pendingKey message.id
    Alarm.scheduleIn r.state (Milliseconds 0.0)
  pure message

react :: Room -> { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
react r { id, emoji, by } = do
  let normalizedEmoji = trim emoji
  when (normalizedEmoji == "") $ fail EmojiRequired
  reactor <- case mkUserName by of
    Left UserNameRequired -> fail ReactorRequired
    Left UserNameTooLong -> fail ReactorTooLong
    Left UserNameInvalid -> fail ReactorInvalid
    Left UserNameReserved -> fail ReactorReserved
    Right accepted -> pure accepted
  let normalizedReactor = printUserName reactor
  liftRuntime (Store.react r.store { id, emoji: normalizedEmoji, by: normalizedReactor }) >>= case _ of
    Nothing -> fail $ NoSuchMessage id
    Just message -> do
      liftRuntime $ Sockets.broadcast r.emit.updated message
      pure message

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
    transcript <- recap <<< takeEnd 20 <$> Store.snapshot r.store
    reply <- maybe (pure $ Left $ Model.Misconfigured "DEEPSEEK_API_KEY is not set") (_ `invoke` transcript) (agentFor r)
    void $ record r
      { author: assistantName
      , images: []
      , replyTo: Just id
      , text: either (\failure -> "(I could not answer: " <> show failure <> ")") identity reply
      }
  where
  recap = joinWith "\n" <<< map \m -> m.author <> ": " <> m.text
