module Chat.Room.Assistant
  ( Assistant
  , hooks
  , open
  , post
  ) where

import Prelude

import Ai (Agent, Def, Model, invoke, mount, text, tool)
import Ai.Exa as Exa
import Ai.Model as Model
import Ai.Schema as Schema
import Chat.Room (Message, NewMessage, RoomEvents, assistantName)
import Chat.Room.Store as Store
import Cloudflare.Durable (Hooks, Runtime, State)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Durable.Sql (Command, Statement)
import Cloudflare.Durable.Sql as Sql
import Data.Array (any, last, nub, takeEnd)
import Data.Codec.Argonaut as CA
import Cloudflare.Durable.Storage as Storage
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Op (Op(..))
import Data.String (joinWith, toLower)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Markdown as Markdown
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console

newtype Assistant = Assistant
  { state :: State
  , store :: Store.Store
  , all :: Sockets (Variant RoomEvents)
  , message :: Sockets Message
  , typing :: Sockets String
  , model :: Maybe (Model Aff)
  , search :: Maybe Exa.Search
  }

type Config =
  { state :: State
  , store :: Store.Store
  , all :: Sockets (Variant RoomEvents)
  , message :: Sockets Message
  , typing :: Sockets String
  , model :: Maybe (Model Aff)
  , search :: Maybe Exa.Search
  }

type Job = { trigger :: Int, attempts :: Int }

data FailureKind
  = ConfigurationFailure
  | TransportFailure
  | ProviderFailure
  | ReplyFailure
  | ToolFailure
  | RoundLimitFailure

pendingKey :: Storage.Key Int
pendingKey = Storage.key "assistant.pending.v2"

legacyPendingKey :: Storage.Key (Array Int)
legacyPendingKey = Storage.key "assistant.pending"

maxAttempts :: Int
maxAttempts = 6

publicFailure :: String
publicFailure = "I could not answer right now."

open :: Config -> Runtime Assistant
open config = do
  let assistant = Assistant config
  migratePending assistant
  resume assistant
  pure assistant

hooks :: Assistant -> Hooks
hooks assistant = Durable.alarmHook $ answer assistant

post :: Assistant -> NewMessage -> Runtime Message
post assistant@(Assistant a) message = do
  let requested = isRequested message
  stored <- if requested then Store.postWith a.store enqueueCommands message else Store.post a.store message
  when requested $ wake assistant
  pure stored

isRequested :: NewMessage -> Boolean
isRequested message =
  toLower message.author /= toLower assistantName
    && any (\mention -> toLower mention == toLower assistantName) (Markdown.mentions message.text)

enqueueCommands :: Array Command
enqueueCommands =
  [ Sql.command deletePending unit
  , Sql.command insertPending unit
  ]

migratePending :: Assistant -> Runtime Unit
migratePending (Assistant a) = do
  pending <- Storage.get a.state pendingKey >>= case _ of
    Just trigger -> pure $ Just trigger
    Nothing -> last <<< fromMaybe [] <$> Storage.get a.state legacyPendingKey
  for_ pending \trigger -> do
    exists <- Store.hasMessage a.store trigger
    when exists do
      updatedAt <- unwrap <<< unInstant <$> Alarm.now a.state
      Sql.batch a.state
        [ Sql.command deletePending unit
        , Sql.command insertLegacyPending { trigger, updatedAt }
        ]
  void $ Storage.delete a.state pendingKey
  void $ Storage.delete a.state legacyPendingKey

resume :: Assistant -> Runtime Unit
resume assistant@(Assistant a) = do
  active <- Sql.first a.state selectActive unit
  for_ active $ const $ wake assistant

wake :: Assistant -> Runtime Unit
wake (Assistant a) = Alarm.scheduleIn a.state $ Milliseconds 0.0

answer :: Assistant -> Runtime Unit
answer assistant@(Assistant a) = do
  updatedAt <- unwrap <<< unInstant <$> Alarm.now a.state
  claimed <- Sql.first a.state claimJob updatedAt
  for_ claimed \job -> do
    Sockets.broadcast a.typing assistantName
    transcript <- recap <<< takeEnd 20 <$> Store.snapshot a.store
    result <- maybe
      (pure $ Left $ Model.Misconfigured "DEEPSEEK_API_KEY is not set")
      (_ `invoke` transcript)
      (agentFor assistant)
    case result of
      Right text -> complete assistant job text
      Left failure -> failOrRetry assistant job failure
  where
  recap = joinWith "\n" <<< map \message -> message.author <> ": " <> message.text

complete :: Assistant -> Job -> String -> Runtime Unit
complete assistant@(Assistant a) job text = do
  message <- Store.assistantReply a.store
    { trigger: job.trigger
    , text
    , status: "completed"
    , failure: Nothing
    }
  Sockets.broadcast a.message message
  resume assistant

failOrRetry :: Assistant -> Job -> Model.AiError -> Runtime Unit
failOrRetry assistant@(Assistant a) job failure = do
  liftEffect $ Console.error $ "assistant job " <> show job.trigger <> " failed: " <> show failure
  let kind = failureKind failure
  if retryable failure && job.attempts < maxAttempts then do
    updatedAt <- unwrap <<< unInstant <$> Alarm.now a.state
    Sql.execute a.state retryJob
      { trigger: job.trigger
      , failure: printFailureKind kind
      , updatedAt
      }
    Alarm.scheduleIn a.state $ retryDelay job.attempts
  else do
    message <- Store.assistantReply a.store
      { trigger: job.trigger
      , text: publicFailure
      , status: "failed"
      , failure: Just $ printFailureKind kind
      }
    Sockets.broadcast a.message message
    resume assistant

retryable :: Model.AiError -> Boolean
retryable = case _ of
  Model.Transport _ -> true
  Model.Rejected { status } -> status == 429 || status >= 500
  _ -> false

retryDelay :: Int -> Milliseconds
retryDelay attempt = Milliseconds case attempt of
  1 -> 1000.0
  2 -> 2000.0
  3 -> 4000.0
  4 -> 8000.0
  5 -> 16000.0
  _ -> 32000.0

failureKind :: Model.AiError -> FailureKind
failureKind = case _ of
  Model.Misconfigured _ -> ConfigurationFailure
  Model.Transport _ -> TransportFailure
  Model.Rejected _ -> ProviderFailure
  Model.BadReply _ -> ReplyFailure
  Model.ToolFailed _ -> ToolFailure
  Model.TooManyRounds _ -> RoundLimitFailure

printFailureKind :: FailureKind -> String
printFailureKind = case _ of
  ConfigurationFailure -> "configuration"
  TransportFailure -> "transport"
  ProviderFailure -> "provider"
  ReplyFailure -> "reply"
  ToolFailure -> "tool"
  RoundLimitFailure -> "round_limit"

agentFor :: Assistant -> Maybe (Agent Runtime String String)
agentFor assistant@(Assistant a) = a.model <#> \model ->
  mount (Model.hoist liftAff model) ([ whoIsHere ] <> foldMap (pure <<< web) a.search) persona
  where
  whoIsHere = tool "members" "Who is in the room right now" (Schema.object {}) (Schema.array Schema.string) \_ -> members assistant
  web search = tool "search" "Search the web; returns pages with a short excerpt of each"
    (Schema.object { query: Schema.describe "What to look for, as you would type it into a search engine" Schema.string })
    (Schema.object { results: Schema.array result, error: Schema.nullable Schema.string })
    \{ query } -> liftAff (search query) <#> either (\why -> { results: [], error: Just why }) (\results -> { results, error: Nothing })
  result = Schema.object { title: Schema.string, url: Schema.string, excerpt: Schema.string }

members :: Assistant -> Runtime (Array String)
members (Assistant a) = nub <<< map _.tag <$> Sockets.connected a.all

persona :: Def String String
persona = text $
  "You are '" <> assistantName <> "', a member of a small chat room. Reply in one or two short sentences, "
    <> "as yourself, to whoever mentioned you last. Markdown is fine. Do not prefix your name. "
    <> "Use the search tool for anything recent or anything you are not sure of, and link the page you relied on."

deletePending :: Statement Unit Unit
deletePending = Sql.statement "DELETE FROM assistant_jobs WHERE status = 'pending'" Sql.noParams (pure unit)

insertPending :: Statement Unit Unit
insertPending = Sql.statement
  "INSERT INTO assistant_jobs (trigger, status, attempts, updated_at) SELECT id, 'pending', 0, sent_at FROM messages ORDER BY id DESC LIMIT 1"
  Sql.noParams
  (pure unit)

insertLegacyPending :: Statement { trigger :: Int, updatedAt :: Number } Unit
insertLegacyPending = Sql.statement
  "INSERT OR IGNORE INTO assistant_jobs (trigger, status, attempts, updated_at) VALUES (?, 'pending', 0, ?)"
  (Op \job -> [ CA.encode CA.int job.trigger, CA.encode CA.number job.updatedAt ])
  (pure unit)

selectActive :: Statement Unit Int
selectActive = Sql.statement
  "SELECT trigger FROM assistant_jobs WHERE status IN ('pending', 'running') ORDER BY trigger DESC LIMIT 1"
  Sql.noParams
  (Sql.columnOf "trigger")

claimJob :: Statement Number Job
claimJob = Sql.statement
  "UPDATE assistant_jobs SET status = 'running', attempts = attempts + 1, updated_at = ? WHERE trigger = COALESCE((SELECT trigger FROM assistant_jobs WHERE status = 'running' ORDER BY trigger DESC LIMIT 1), (SELECT trigger FROM assistant_jobs WHERE status = 'pending' ORDER BY trigger DESC LIMIT 1)) RETURNING trigger, attempts"
  (Sql.param CA.number)
  ({ trigger: _, attempts: _ } <$> Sql.columnOf "trigger" <*> Sql.columnOf "attempts")

retryJob :: Statement { trigger :: Int, failure :: String, updatedAt :: Number } Unit
retryJob = Sql.statement
  "UPDATE assistant_jobs SET status = 'pending', failure = ?, updated_at = ? WHERE trigger = ? AND status = 'running'"
  ( Op \job ->
      [ CA.encode CA.string job.failure
      , CA.encode CA.number job.updatedAt
      , CA.encode CA.int job.trigger
      ]
  )
  (pure unit)
