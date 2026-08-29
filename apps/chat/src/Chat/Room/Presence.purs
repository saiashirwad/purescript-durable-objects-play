module Chat.Room.Presence
  ( Presence
  , hooks
  , members
  , open
  ) where

import Prelude

import Chat.Room (RoomEvents)
import Cloudflare.Durable (Hooks, Runtime)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Runtime (class MonadRuntime)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Data.Array (filter, nub)
import Data.Variant (Variant)

newtype Presence = Presence
  { all :: Sockets (Variant RoomEvents)
  , emit :: Sockets (Array String)
  }

open :: Sockets (Variant RoomEvents) -> Sockets (Array String) -> Presence
open all emit = Presence { all, emit }

members :: forall m. MonadRuntime m => Presence -> m (Array String)
members (Presence presence) = nub <<< map _.tag <$> Sockets.connected presence.all

hooks :: Presence -> Hooks
hooks presence =
  Durable.connectHook (const $ broadcast presence)
    <> Durable.disconnectHook (\socket -> broadcastLeaving presence socket.id)

broadcast :: Presence -> Runtime Unit
broadcast presence@(Presence channels) = members presence >>= Sockets.broadcast channels.emit

broadcastLeaving :: Presence -> String -> Runtime Unit
broadcastLeaving (Presence channels) leaving = do
  connected <- Sockets.connected channels.all
  let remaining = filter (\socket -> socket.id /= leaving) connected
  Sockets.broadcast channels.emit $ nub $ map _.tag remaining
