-- | The chat room contract. Browsers and Workers import this; only the host
-- | imports `Chat.Room.Live`.
module Chat.Room
  ( Message
  , NewMessage
  , PostError(..)
  , RoomApi
  , maxTextLength
  , room
  ) where

import Prelude

import Cloudflare.Durable (Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Codec (class HasCodec)
import Cloudflare.Durable.Rpc (NoError, Rpc, method)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))

type Message =
  { id :: Int
  , author :: String
  , text :: String
  , sentAt :: Number
  }

type NewMessage = { author :: String, text :: String }

data PostError
  = AuthorRequired
  | TextRequired
  | TextTooLong

derive instance eqPostError :: Eq PostError

instance showPostError :: Show PostError where
  show = case _ of
    AuthorRequired -> "AuthorRequired"
    TextRequired -> "TextRequired"
    TextTooLong -> "TextTooLong"

instance hasCodecPostError :: HasCodec PostError where
  codec = CA.prismaticCodec "PostError" parse show CA.string
    where
    parse = case _ of
      "AuthorRequired" -> Just AuthorRequired
      "TextRequired" -> Just TextRequired
      "TextTooLong" -> Just TextTooLong
      _ -> Nothing

maxTextLength :: Int
maxTextLength = 500

-- | `since n` returns the messages with an id above `n`. If there are none
-- | yet it waits for the next post, or gives up empty after a while. Call it
-- | in a loop to follow the room.
type RoomApi =
  ( post :: NewMessage -> Rpc PostError Message
  , history :: Unit -> Rpc NoError (Array Message)
  , since :: Int -> Rpc NoError (Array Message)
  )

room :: Object "Room" RoomApi
room = Durable.object
  { post: method
  , history: method
  , since: method
  }
