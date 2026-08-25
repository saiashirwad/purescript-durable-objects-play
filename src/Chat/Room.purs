module Chat.Room
  ( Message
  , NewMessage
  , PostError(..)
  , ReactError(..)
  , Reaction
  , RoomApi
  , RoomEvents
  , maxTextLength
  , room
  ) where

import Prelude

import Cloudflare.Durable (Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Codec (class HasCodec)
import Cloudflare.Durable.Rpc (NoError, Rpc, method)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))

type Reaction = { emoji :: String, by :: Array String }

-- | `text` is Markdown. `images` are ids served by the room at `/image/<id>`.
-- | `mentions` are the `@names` the server found in the text.
type Message =
  { id :: Int
  , author :: String
  , text :: String
  , images :: Array Int
  , replyTo :: Maybe Int
  , mentions :: Array String
  , reactions :: Array Reaction
  , sentAt :: Number
  }

type NewMessage = { author :: String, text :: String, images :: Array Int, replyTo :: Maybe Int }

data PostError
  = AuthorRequired
  | TextRequired
  | TextTooLong
  | NoSuchReply Int
  | NoSuchImage Int

derive instance eqPostError :: Eq PostError

instance showPostError :: Show PostError where
  show = case _ of
    AuthorRequired -> "AuthorRequired"
    TextRequired -> "TextRequired"
    TextTooLong -> "TextTooLong"
    NoSuchReply id -> "(NoSuchReply " <> show id <> ")"
    NoSuchImage id -> "(NoSuchImage " <> show id <> ")"

-- | `{ tag, id }` on the wire; a sum with one payload needs no more.
instance hasCodecPostError :: HasCodec PostError where
  codec = CA.prismaticCodec "PostError" from to $ CAR.object "PostError" { tag: CA.string, id: CAR.optional CA.int }
    where
    from = case _ of
      { tag: "AuthorRequired" } -> Just AuthorRequired
      { tag: "TextRequired" } -> Just TextRequired
      { tag: "TextTooLong" } -> Just TextTooLong
      { tag: "NoSuchReply", id: Just id } -> Just (NoSuchReply id)
      { tag: "NoSuchImage", id: Just id } -> Just (NoSuchImage id)
      _ -> Nothing
    to = case _ of
      AuthorRequired -> { tag: "AuthorRequired", id: Nothing }
      TextRequired -> { tag: "TextRequired", id: Nothing }
      TextTooLong -> { tag: "TextTooLong", id: Nothing }
      NoSuchReply id -> { tag: "NoSuchReply", id: Just id }
      NoSuchImage id -> { tag: "NoSuchImage", id: Just id }

data ReactError = NoSuchMessage Int | EmojiRequired

derive instance eqReactError :: Eq ReactError

instance showReactError :: Show ReactError where
  show = case _ of
    NoSuchMessage id -> "(NoSuchMessage " <> show id <> ")"
    EmojiRequired -> "EmojiRequired"

instance hasCodecReactError :: HasCodec ReactError where
  codec = CA.prismaticCodec "ReactError" from to $ CAR.object "ReactError" { tag: CA.string, id: CAR.optional CA.int }
    where
    from = case _ of
      { tag: "NoSuchMessage", id: Just id } -> Just (NoSuchMessage id)
      { tag: "EmojiRequired" } -> Just EmojiRequired
      _ -> Nothing
    to = case _ of
      NoSuchMessage id -> { tag: "NoSuchMessage", id: Just id }
      EmojiRequired -> { tag: "EmojiRequired", id: Nothing }

maxTextLength :: Int
maxTextLength = 4000

type RoomApi =
  ( post :: NewMessage -> Rpc PostError Message
  , react :: { id :: Int, emoji :: String, by :: String } -> Rpc ReactError Message
  , history :: Unit -> Rpc NoError (Array Message)
  , members :: Unit -> Rpc NoError (Array String)
  , typing :: String -> Rpc NoError Unit
  )

-- | Pushed to every open socket. `updated` carries a message whose
-- | reactions changed; `joined`, `left` and `typing` carry a name.
type RoomEvents =
  ( message :: Message
  , updated :: Message
  , joined :: String
  , left :: String
  , typing :: String
  )

room :: Object "Room" RoomApi RoomEvents
room =
  Durable.object { post: method, react: method, history: method, members: method, typing: method }
    `Durable.emitting`
      { message: Durable.event, updated: Durable.event, joined: Durable.event, left: Durable.event, typing: Durable.event }
