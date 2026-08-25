module Chat.Room
  ( Message
  , NewMessage
  , PostError(..)
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

type RoomApi =
  ( post :: NewMessage -> Rpc PostError Message
  , history :: Unit -> Rpc NoError (Array Message)
  , members :: Unit -> Rpc NoError (Array String)
  )

-- | Pushed to every open socket. `joined` and `left` carry the socket's tag,
-- | which the chat uses for the author's name.
type RoomEvents =
  ( message :: Message
  , joined :: String
  , left :: String
  )

room :: Object "Room" RoomApi RoomEvents
room =
  Durable.object { post: method, history: method, members: method }
    `Durable.emitting` { message: Durable.event, joined: Durable.event, left: Durable.event }
