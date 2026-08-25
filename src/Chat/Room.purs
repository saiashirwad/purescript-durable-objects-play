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
import Data.Codec.Argonaut.Generic (nullarySum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
derive instance genericPostError :: Generic PostError _

instance showPostError :: Show PostError where
  show = genericShow

instance hasCodecPostError :: HasCodec PostError where
  codec = nullarySum "PostError"

maxTextLength :: Int
maxTextLength = 500

-- | `since n` returns messages with id above `n`, waiting for the next post
-- | if there are none; after 20 seconds it returns empty.
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
