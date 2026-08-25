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
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

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

-- | `{ tag, id }` on the wire; a sum with at most one `Int` payload needs no
-- | more. `prismaticCodec` is the codec of a partial isomorphism: every
-- | value prints, only well-formed wire values read.
instance hasCodecPostError :: HasCodec PostError where
  codec = tagged "PostError" print read
    where
    print = case _ of
      AuthorRequired -> "AuthorRequired" /\ Nothing
      TextRequired -> "TextRequired" /\ Nothing
      TextTooLong -> "TextTooLong" /\ Nothing
      NoSuchReply id -> "NoSuchReply" /\ Just id
      NoSuchImage id -> "NoSuchImage" /\ Just id
    read = case _ of
      "AuthorRequired" /\ _ -> Just AuthorRequired
      "TextRequired" /\ _ -> Just TextRequired
      "TextTooLong" /\ _ -> Just TextTooLong
      "NoSuchReply" /\ Just id -> Just (NoSuchReply id)
      "NoSuchImage" /\ Just id -> Just (NoSuchImage id)
      _ -> Nothing

data ReactError = NoSuchMessage Int | EmojiRequired

derive instance eqReactError :: Eq ReactError

instance showReactError :: Show ReactError where
  show = case _ of
    NoSuchMessage id -> "(NoSuchMessage " <> show id <> ")"
    EmojiRequired -> "EmojiRequired"

instance hasCodecReactError :: HasCodec ReactError where
  codec = tagged "ReactError" print read
    where
    print = case _ of
      NoSuchMessage id -> "NoSuchMessage" /\ Just id
      EmojiRequired -> "EmojiRequired" /\ Nothing
    read = case _ of
      "NoSuchMessage" /\ Just id -> Just (NoSuchMessage id)
      "EmojiRequired" /\ _ -> Just EmojiRequired
      _ -> Nothing

tagged :: forall a. String -> (a -> Tuple String (Maybe Int)) -> (Tuple String (Maybe Int) -> Maybe a) -> JsonCodec a
tagged name print read =
  CA.prismaticCodec name (read <<< toPair) (fromPair <<< print) $ CAR.object name { tag: CA.string, id: CAR.optional CA.int }
  where
  toPair { tag, id } = tag /\ id
  fromPair (tag /\ id) = { tag, id }

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
