module Chat.Room.Migrations
  ( LegacyMessage
  , initialize
  ) where

import Prelude

import Chat.Room (Message, Reaction, assistantName)
import Cloudflare.Durable (Runtime, State)
import Cloudflare.Durable.Sql (Command, Statement)
import Cloudflare.Durable.Sql as Sql
import Cloudflare.Durable.Storage as Storage
import Data.Array (concatMap, mapWithIndex, null)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Maybe (Maybe(..))
import Data.Op (Op(..))
import Data.String (toLower)
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
  Sql.execute state createMessageImages unit
  Sql.execute state createReactions unit
  Sql.execute state createMessageImagesIndex unit
  Sql.execute state createReactionsIndex unit
  count <- Sql.one state countMessages unit
  if count > 0 then deleteLegacyKeys state
  else Storage.get state messagesKey >>= case _ of
    Just messages -> importMessages state messages *> deleteLegacyKeys state
    Nothing -> Storage.get state legacyKey >>= case _ of
      Just legacy -> importMessages state (upgrade <$> legacy) *> deleteLegacyKeys state
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

reactionCommands :: Int -> Reaction -> Array Command
reactionCommands messageId reaction = reaction.by <#> \reactor ->
  Sql.command insertReaction { messageId, emoji: reaction.emoji, reactor }

upgrade :: LegacyMessage -> Message
upgrade message =
  { id: message.id
  , author: message.author
  , text: message.text
  , images: []
  , replyTo: Nothing
  , mentions: Markdown.mentions message.text
  , reactions: []
  , sentAt: message.sentAt
  }

deleteLegacyKeys :: State -> Runtime Unit
deleteLegacyKeys state = do
  void $ Storage.delete state messagesKey
  void $ Storage.delete state legacyKey

createMessages :: Statement Unit Unit
createMessages = Sql.statement
  "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, author_kind TEXT NOT NULL CHECK (author_kind IN ('human', 'assistant')), author_name TEXT NOT NULL, text TEXT NOT NULL, reply_to INTEGER, sent_at REAL NOT NULL)"
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

countMessages :: Statement Unit Int
countMessages = Sql.statement "SELECT COUNT(*) AS count FROM messages" Sql.noParams (Sql.columnOf "count")

insertMessage :: Statement Message Unit
insertMessage = Sql.statement
  "INSERT OR IGNORE INTO messages (id, author_kind, author_name, text, reply_to, sent_at) VALUES (?, ?, ?, ?, ?, ?)"
  ( Op \message ->
      [ CA.encode CA.int message.id
      , CA.encode CA.string $ if toLower message.author == assistantName then "assistant" else "human"
      , CA.encode CA.string $ if toLower message.author == assistantName then "" else message.author
      , CA.encode CA.string message.text
      , CA.encode (Compat.maybe CA.int) message.replyTo
      , CA.encode CA.number message.sentAt
      ]
  )
  (pure unit)

insertImageLink :: Statement { messageId :: Int, imageId :: Int, position :: Int } Unit
insertImageLink = Sql.statement
  "INSERT OR IGNORE INTO message_images (message_id, image_id, position) VALUES (?, ?, ?)"
  (Op \link -> [ CA.encode CA.int link.messageId, CA.encode CA.int link.imageId, CA.encode CA.int link.position ])
  (pure unit)

insertReaction :: Statement { messageId :: Int, emoji :: String, reactor :: String } Unit
insertReaction = Sql.statement
  "INSERT OR IGNORE INTO reactions (message_id, emoji, reactor) VALUES (?, ?, ?)"
  (Op \reaction -> [ CA.encode CA.int reaction.messageId, CA.encode CA.string reaction.emoji, CA.encode CA.string reaction.reactor ])
  (pure unit)
