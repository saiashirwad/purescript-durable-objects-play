module Chat.Room.Store
  ( Store
  , assistantReply
  , find
  , findAssistantReply
  , hasMessage
  , open
  , post
  , postWith
  , react
  , snapshot
  ) where

import Prelude

import Chat.Room (Message, NewMessage, Reaction, assistantName)
import Chat.Room.Migrations as Migrations
import Cloudflare.Durable (Runtime, State)
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Runtime (platformError)
import Cloudflare.Durable.Sql (Command, Decoder, Statement)
import Data.Codec.Argonaut.Compat as Compat
import Cloudflare.Durable.Sql as Sql
import Data.Array (findIndex, modifyAt, snoc)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.DateTime.Instant (unInstant)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Op (Op(..))
import Data.String (toLower)
import Markdown as Markdown

newtype Store = Store State

type StoredMessage =
  { id :: Int
  , author :: String
  , text :: String
  , replyTo :: Maybe Int
  , sentAt :: Number
  }

type ImageRow = { messageId :: Int, imageId :: Int }

type ReactionRow = { messageId :: Int, emoji :: String, reactor :: String }

open :: State -> Runtime Store
open state = Migrations.initialize state $> Store state

snapshot :: Store -> Runtime (Array Message)
snapshot (Store state) = do
  messages <- Sql.query state selectMessages unit
  images <- Sql.query state selectImages unit
  reactions <- Sql.query state selectReactions unit
  pure $ hydrate messages images reactions

find :: Store -> Int -> Runtime (Maybe Message)
find (Store state) id = Sql.first state selectMessage id >>= case _ of
  Nothing -> pure Nothing
  Just message -> do
    images <- Sql.query state selectMessageImages id
    reactions <- Sql.query state selectMessageReactions id
    pure $ Array.head $ hydrate [ message ] images reactions

hasMessage :: Store -> Int -> Runtime Boolean
hasMessage store id = find store id <#> case _ of
  Just _ -> true
  Nothing -> false

post :: Store -> NewMessage -> Runtime Message
post store = postWith store []

postWith :: Store -> Array Command -> NewMessage -> Runtime Message
postWith store@(Store state) extra message = do
  sentAt <- unwrap <<< unInstant <$> Alarm.now state
  let
    commands =
      Array.mapWithIndex (\position imageId -> Sql.command insertImageLink { imageId, position }) message.images
        <> extra
        <> retentionCommands
  id <- Sql.batchOne state insertMessage { message, sentAt } commands
  find store id >>= maybe (platformError "room post" "inserted message is missing") pure

findAssistantReply :: Store -> Int -> Runtime (Maybe Message)
findAssistantReply store@(Store state) trigger =
  Sql.first state selectAssistantReplyId trigger >>= case _ of
    Nothing -> pure Nothing
    Just id -> find store id

assistantReply
  :: Store
  -> { trigger :: Int, text :: String, status :: String, failure :: Maybe String }
  -> Runtime Message
assistantReply store@(Store state) result = findAssistantReply store result.trigger >>= case _ of
  Just existing -> pure existing
  Nothing -> do
    sentAt <- unwrap <<< unInstant <$> Alarm.now state
    let
      finish = Sql.command finishAssistantJob
        { trigger: result.trigger
        , status: result.status
        , failure: result.failure
        , updatedAt: sentAt
        }
    id <- Sql.batchOne state insertAssistantMessage
      { trigger: result.trigger, text: result.text, sentAt }
      ([ finish ] <> retentionCommands)
    find store id >>= maybe (platformError "assistant reply" "inserted reply is missing") pure

react :: Store -> { id :: Int, emoji :: String, by :: String } -> Runtime (Maybe Message)
react store@(Store state) reaction = find store reaction.id >>= case _ of
  Nothing -> pure Nothing
  Just _ -> do
    active <- Sql.first state reactionExists reaction
    case active of
      Just _ -> Sql.execute state deleteReaction reaction
      Nothing -> Sql.execute state insertReaction reaction
    find store reaction.id

hydrate :: Array StoredMessage -> Array ImageRow -> Array ReactionRow -> Array Message
hydrate stored imageRows reactionRows = stored <#> \message ->
  { id: message.id
  , author: message.author
  , text: message.text
  , images: fromMaybe [] $ Map.lookup message.id images
  , replyTo: message.replyTo
  , mentions: Markdown.mentions message.text
  , reactions: fromMaybe [] $ Map.lookup message.id reactions
  , sentAt: message.sentAt
  }
  where
  images = foldl (\all row -> Map.alter (Just <<< maybe [ row.imageId ] (_ `snoc` row.imageId)) row.messageId all) Map.empty imageRows
  reactions = foldl addReaction Map.empty reactionRows

addReaction :: Map Int (Array Reaction) -> ReactionRow -> Map Int (Array Reaction)
addReaction all row = Map.alter (Just <<< maybe [ next ] append) row.messageId all
  where
  next = { emoji: row.emoji, by: [ row.reactor ] }
  append existing = case findIndex (_.emoji >>> eq row.emoji) existing of
    Nothing -> snoc existing next
    Just index -> fromMaybe existing $ modifyAt index (\reaction -> reaction { by = snoc reaction.by row.reactor }) existing

storedMessage :: Decoder StoredMessage
storedMessage =
  { id: _
  , author: _
  , text: _
  , replyTo: _
  , sentAt: _
  }
    <$> Sql.columnOf "id"
    <*> Sql.columnOf "author"
    <*> Sql.columnOf "text"
    <*> Sql.column "reply_to" (Compat.maybe CA.int)
    <*> Sql.columnOf "sent_at"

selectMessages :: Statement Unit StoredMessage
selectMessages = Sql.statement
  "SELECT id, CASE author_kind WHEN 'assistant' THEN 'ai' ELSE author_name END AS author, text, reply_to, sent_at FROM (SELECT * FROM messages ORDER BY id DESC LIMIT 500) ORDER BY id"
  Sql.noParams
  storedMessage

selectMessage :: Statement Int StoredMessage
selectMessage = Sql.statement
  "SELECT id, CASE author_kind WHEN 'assistant' THEN 'ai' ELSE author_name END AS author, text, reply_to, sent_at FROM messages WHERE id = ?"
  Sql.paramOf
  storedMessage

selectImages :: Statement Unit ImageRow
selectImages = Sql.statement
  "SELECT message_id, image_id FROM message_images ORDER BY message_id, position"
  Sql.noParams
  ({ messageId: _, imageId: _ } <$> Sql.columnOf "message_id" <*> Sql.columnOf "image_id")

selectMessageImages :: Statement Int ImageRow
selectMessageImages = Sql.statement
  "SELECT message_id, image_id FROM message_images WHERE message_id = ? ORDER BY position"
  Sql.paramOf
  ({ messageId: _, imageId: _ } <$> Sql.columnOf "message_id" <*> Sql.columnOf "image_id")

selectReactions :: Statement Unit ReactionRow
selectReactions = Sql.statement
  "SELECT message_id, emoji, reactor FROM reactions ORDER BY rowid"
  Sql.noParams
  ({ messageId: _, emoji: _, reactor: _ } <$> Sql.columnOf "message_id" <*> Sql.columnOf "emoji" <*> Sql.columnOf "reactor")

selectMessageReactions :: Statement Int ReactionRow
selectMessageReactions = Sql.statement
  "SELECT message_id, emoji, reactor FROM reactions WHERE message_id = ? ORDER BY rowid"
  Sql.paramOf
  ({ messageId: _, emoji: _, reactor: _ } <$> Sql.columnOf "message_id" <*> Sql.columnOf "emoji" <*> Sql.columnOf "reactor")

insertMessage :: Statement { message :: NewMessage, sentAt :: Number } Int
insertMessage = Sql.statement
  "INSERT INTO messages (author_kind, author_name, text, reply_to, sent_at) VALUES (?, ?, ?, ?, ?) RETURNING id"
  ( Op \{ message, sentAt } ->
      [ CA.encode CA.string $ if toLower message.author == assistantName then "assistant" else "human"
      , CA.encode CA.string $ if toLower message.author == assistantName then "" else message.author
      , CA.encode CA.string message.text
      , CA.encode (Compat.maybe CA.int) message.replyTo
      , CA.encode CA.number sentAt
      ]
  )
  (Sql.columnOf "id")

selectAssistantReplyId :: Statement Int Int
selectAssistantReplyId = Sql.statement
  "SELECT id FROM messages WHERE assistant_trigger = ?"
  Sql.paramOf
  (Sql.columnOf "id")

insertAssistantMessage :: Statement { trigger :: Int, text :: String, sentAt :: Number } Int
insertAssistantMessage = Sql.statement
  "INSERT INTO messages (author_kind, author_name, text, reply_to, sent_at, assistant_trigger) VALUES ('assistant', '', ?, ?, ?, ?) RETURNING id"
  ( Op \reply ->
      [ CA.encode CA.string reply.text
      , CA.encode CA.int reply.trigger
      , CA.encode CA.number reply.sentAt
      , CA.encode CA.int reply.trigger
      ]
  )
  (Sql.columnOf "id")

finishAssistantJob :: Statement { trigger :: Int, status :: String, failure :: Maybe String, updatedAt :: Number } Unit
finishAssistantJob = Sql.statement
  "UPDATE assistant_jobs SET status = ?, reply_id = (SELECT MAX(id) FROM messages), failure = ?, updated_at = ? WHERE trigger = ?"
  ( Op \job ->
      [ CA.encode CA.string job.status
      , CA.encode (Compat.maybe CA.string) job.failure
      , CA.encode CA.number job.updatedAt
      , CA.encode CA.int job.trigger
      ]
  )
  (pure unit)

insertImageLink :: Statement { imageId :: Int, position :: Int } Unit
insertImageLink = Sql.statement
  "INSERT INTO message_images (message_id, image_id, position) VALUES ((SELECT MAX(id) FROM messages), ?, ?)"
  (Op \link -> [ CA.encode CA.int link.imageId, CA.encode CA.int link.position ])
  (pure unit)

reactionExists :: Statement { id :: Int, emoji :: String, by :: String } Int
reactionExists = Sql.statement
  "SELECT 1 AS found FROM reactions WHERE message_id = ? AND emoji = ? AND reactor = ?"
  reactionParams
  (Sql.columnOf "found")

insertReaction :: Statement { id :: Int, emoji :: String, by :: String } Unit
insertReaction = Sql.statement
  "INSERT INTO reactions (message_id, emoji, reactor) VALUES (?, ?, ?)"
  reactionParams
  (pure unit)

deleteReaction :: Statement { id :: Int, emoji :: String, by :: String } Unit
deleteReaction = Sql.statement
  "DELETE FROM reactions WHERE message_id = ? AND emoji = ? AND reactor = ?"
  reactionParams
  (pure unit)

reactionParams :: Sql.Params { id :: Int, emoji :: String, by :: String }
reactionParams = Op \reaction -> [ CA.encode CA.int reaction.id, CA.encode CA.string reaction.emoji, CA.encode CA.string reaction.by ]

retentionCommands :: Array Command
retentionCommands =
  [ Sql.command deleteExpiredImages unit
  , Sql.command deleteExpiredImageLinks unit
  , Sql.command deleteExpiredReactions unit
  , Sql.command deleteExpiredAssistantJobs unit
  , Sql.command deleteExpiredMessages unit
  ]

expiredMessages :: String
expiredMessages = "SELECT id FROM messages ORDER BY id DESC LIMIT -1 OFFSET 500"

deleteExpiredImages :: Statement Unit Unit
deleteExpiredImages = Sql.statement
  ("DELETE FROM images WHERE id IN (SELECT image_id FROM message_images WHERE message_id IN (" <> expiredMessages <> ")) AND id NOT IN (SELECT image_id FROM message_images WHERE message_id NOT IN (" <> expiredMessages <> "))")
  Sql.noParams
  (pure unit)

deleteExpiredImageLinks :: Statement Unit Unit
deleteExpiredImageLinks = Sql.statement
  ("DELETE FROM message_images WHERE message_id IN (" <> expiredMessages <> ")")
  Sql.noParams
  (pure unit)

deleteExpiredReactions :: Statement Unit Unit
deleteExpiredReactions = Sql.statement
  ("DELETE FROM reactions WHERE message_id IN (" <> expiredMessages <> ")")
  Sql.noParams
  (pure unit)

deleteExpiredAssistantJobs :: Statement Unit Unit
deleteExpiredAssistantJobs = Sql.statement
  ("DELETE FROM assistant_jobs WHERE trigger IN (" <> expiredMessages <> ")")
  Sql.noParams
  (pure unit)

deleteExpiredMessages :: Statement Unit Unit
deleteExpiredMessages = Sql.statement
  ("DELETE FROM messages WHERE id IN (" <> expiredMessages <> ")")
  Sql.noParams
  (pure unit)
