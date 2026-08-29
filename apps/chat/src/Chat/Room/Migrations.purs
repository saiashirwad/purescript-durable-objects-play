module Chat.Room.Migrations
  ( LegacyMessage
  , initialize
  ) where

import Prelude

import Chat.Room (ImageId, Message, MessageId, Reaction, isAssistant, mkAuthor, mkMessageId, printAuthor)
import Cloudflare.Durable (Runtime, State)
import Cloudflare.Durable.Codec (codec)
import Cloudflare.Durable.Sql (Command, Statement)
import Cloudflare.Durable.Sql as Sql
import Cloudflare.Durable.Runtime (platformError)
import Cloudflare.Durable.Storage as Storage
import Data.Array (any, concatMap, mapWithIndex, null)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Op (Op(..))
import Data.Traversable (traverse)
import Markdown as Markdown

type LegacyMessage =
  { id :: Int
  , author :: String
  , text :: String
  , sentAt :: Number
  }

messagesKey :: Storage.Key (Array Message)
messagesKey = Storage.key "messages.v2"

legacyKey :: Storage.Key (Array LegacyMessage)
legacyKey = Storage.key "messages"

initialize :: State -> Runtime Unit
initialize state = do
  Sql.execute state createMessages unit
  columns <- Sql.query state messageColumns unit
  unless (any (_ == "assistant_trigger") columns) $ Sql.execute state addAssistantTrigger unit
  Sql.execute state createAssistantTriggerIndex unit
  Sql.execute state createMessageImages unit
  Sql.execute state createReactions unit
  Sql.execute state createAssistantJobs unit
  Sql.execute state createMessageImagesIndex unit
  Sql.execute state createReactionsIndex unit
  Sql.execute state createAssistantJobsIndex unit
  count <- Sql.one state countMessages unit
  if count > 0 then deleteLegacyKeys state
  else Storage.get state messagesKey >>= case _ of
    Just messages -> importMessages state messages *> deleteLegacyKeys state
    Nothing -> Storage.get state legacyKey >>= case _ of
      Just legacy -> do
        messages <- traverse upgrade legacy
        importMessages state messages
        deleteLegacyKeys state
      Nothing -> pure unit

importMessages :: State -> Array Message -> Runtime Unit
importMessages state messages = unless (null commands) $ Sql.batch state commands
  where
  commands = concatMap messageCommands messages

messageCommands :: Message -> Array Command
messageCommands message =
  [ Sql.command insertMessage message ]
    <> mapWithIndex (\position imageId -> Sql.command insertImageLink { messageId: message.id, imageId, position }) message.images
    <> concatMap (reactionCommands message.id) message.reactions

reactionCommands :: MessageId -> Reaction -> Array Command
reactionCommands messageId reaction = reaction.by <#> \reactor ->
  Sql.command insertReaction { messageId, emoji: reaction.emoji, reactor }

upgrade :: LegacyMessage -> Runtime Message
upgrade message = case mkMessageId message.id, mkAuthor message.author of
  Just id, Right author -> pure
    { id
    , author
    , text: message.text
    , images: []
    , replyTo: Nothing
    , mentions: Markdown.mentions message.text
    , reactions: []
    , sentAt: message.sentAt
    }
  Nothing, _ -> platformError "room migration" $ "invalid legacy message id " <> show message.id
  _, Left why -> platformError "room migration" $ "invalid legacy author: " <> show why

deleteLegacyKeys :: State -> Runtime Unit
deleteLegacyKeys state = do
  void $ Storage.delete state messagesKey
  void $ Storage.delete state legacyKey

createMessages :: Statement Unit Unit
createMessages = Sql.statement
  "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, author_kind TEXT NOT NULL CHECK (author_kind IN ('human', 'assistant')), author_name TEXT NOT NULL, text TEXT NOT NULL, reply_to INTEGER, sent_at REAL NOT NULL, assistant_trigger INTEGER)"
  Sql.noParams
  (pure unit)

messageColumns :: Statement Unit String
messageColumns = Sql.statement "PRAGMA table_info(messages)" Sql.noParams (Sql.columnOf "name")

addAssistantTrigger :: Statement Unit Unit
addAssistantTrigger = Sql.statement "ALTER TABLE messages ADD COLUMN assistant_trigger INTEGER" Sql.noParams (pure unit)

createAssistantTriggerIndex :: Statement Unit Unit
createAssistantTriggerIndex = Sql.statement
  "CREATE UNIQUE INDEX IF NOT EXISTS messages_assistant_trigger ON messages (assistant_trigger) WHERE assistant_trigger IS NOT NULL"
  Sql.noParams
  (pure unit)

createMessageImages :: Statement Unit Unit
createMessageImages = Sql.statement
  "CREATE TABLE IF NOT EXISTS message_images (message_id INTEGER NOT NULL, image_id INTEGER NOT NULL, position INTEGER NOT NULL, PRIMARY KEY (message_id, position))"
  Sql.noParams
  (pure unit)

createReactions :: Statement Unit Unit
createReactions = Sql.statement
  "CREATE TABLE IF NOT EXISTS reactions (message_id INTEGER NOT NULL, emoji TEXT NOT NULL, reactor TEXT NOT NULL, PRIMARY KEY (message_id, emoji, reactor))"
  Sql.noParams
  (pure unit)

createAssistantJobs :: Statement Unit Unit
createAssistantJobs = Sql.statement
  "CREATE TABLE IF NOT EXISTS assistant_jobs (trigger INTEGER PRIMARY KEY, status TEXT NOT NULL CHECK (status IN ('pending', 'running', 'completed', 'failed')), attempts INTEGER NOT NULL DEFAULT 0, reply_id INTEGER UNIQUE, failure TEXT, updated_at REAL NOT NULL)"
  Sql.noParams
  (pure unit)

createMessageImagesIndex :: Statement Unit Unit
createMessageImagesIndex = Sql.statement
  "CREATE INDEX IF NOT EXISTS message_images_image_id ON message_images (image_id)"
  Sql.noParams
  (pure unit)

createReactionsIndex :: Statement Unit Unit
createReactionsIndex = Sql.statement
  "CREATE INDEX IF NOT EXISTS reactions_message_id ON reactions (message_id)"
  Sql.noParams
  (pure unit)

createAssistantJobsIndex :: Statement Unit Unit
createAssistantJobsIndex = Sql.statement
  "CREATE INDEX IF NOT EXISTS assistant_jobs_status_trigger ON assistant_jobs (status, trigger DESC)"
  Sql.noParams
  (pure unit)

countMessages :: Statement Unit Int
countMessages = Sql.statement "SELECT COUNT(*) AS count FROM messages" Sql.noParams (Sql.columnOf "count")

insertMessage :: Statement Message Unit
insertMessage = Sql.statement
  "INSERT OR IGNORE INTO messages (id, author_kind, author_name, text, reply_to, sent_at) VALUES (?, ?, ?, ?, ?, ?)"
  ( Op \message ->
      [ CA.encode codec message.id
      , CA.encode CA.string $ if isAssistant message.author then "assistant" else "human"
      , CA.encode CA.string $ if isAssistant message.author then "" else printAuthor message.author
      , CA.encode CA.string message.text
      , CA.encode (Compat.maybe codec) message.replyTo
      , CA.encode CA.number message.sentAt
      ]
  )
  (pure unit)

insertImageLink :: Statement { messageId :: MessageId, imageId :: ImageId, position :: Int } Unit
insertImageLink = Sql.statement
  "INSERT OR IGNORE INTO message_images (message_id, image_id, position) VALUES (?, ?, ?)"
  (Op \link -> [ CA.encode codec link.messageId, CA.encode codec link.imageId, CA.encode CA.int link.position ])
  (pure unit)

insertReaction :: Statement { messageId :: MessageId, emoji :: String, reactor :: String } Unit
insertReaction = Sql.statement
  "INSERT OR IGNORE INTO reactions (message_id, emoji, reactor) VALUES (?, ?, ?)"
  (Op \reaction -> [ CA.encode codec reaction.messageId, CA.encode CA.string reaction.emoji, CA.encode CA.string reaction.reactor ])
  (pure unit)
