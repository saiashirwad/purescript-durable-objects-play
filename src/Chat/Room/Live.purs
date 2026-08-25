module Chat.Room.Live
  ( roomLive
  ) where

import Prelude

import Chat.Room (Message, PostError(..), RoomApi, RoomEvents, maxTextLength, room)
import Cloudflare.Durable (Live)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (fail)
import Cloudflare.Durable.Sockets (Sockets)
import Cloudflare.Durable.Sockets as Sockets
import Cloudflare.Durable.Storage as Storage
import Data.Array (last, nub, snoc, takeEnd)
import Data.DateTime.Instant (unInstant)
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (length, trim)
import Data.Functor.Contravariant (cmap)
import Data.Variant (inj)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

messagesKey :: Storage.Key (Array Message)
messagesKey = Storage.key "messages"

keptMessages :: Int
keptMessages = 500

roomLive :: Live "Room" RoomApi RoomEvents
roomLive =
  Durable.implementWith room ado
    state <- Durable.state
    sockets <- Durable.sockets room
    in
      do
        stored <- Storage.get state messagesKey
        messages <- liftEffect $ Ref.new $ fromMaybe [] stored
        let
          -- One channel per event: `cmap` narrows the socket to that case.
          posted = cmap (inj (Proxy :: Proxy "message")) sockets :: Sockets Message
          joined = cmap (inj (Proxy :: Proxy "joined")) sockets :: Sockets String
          left = cmap (inj (Proxy :: Proxy "left")) sockets :: Sockets String
          typing = cmap (inj (Proxy :: Proxy "typing")) sockets :: Sockets String
        pure
          { methods:
              { post: \new -> do
                  let author = trim new.author
                  let text = trim new.text
                  when (author == "") $ fail AuthorRequired
                  when (text == "") $ fail TextRequired
                  when (length text > maxTextLength) $ fail TextTooLong
                  sentAt <- liftEffect $ unwrap <<< unInstant <$> now
                  all <- liftEffect $ Ref.read messages
                  let message = { id: maybe 1 (\m -> m.id + 1) (last all), author, text, sentAt }
                  let kept = takeEnd keptMessages $ snoc all message
                  Storage.put state messagesKey kept
                  liftEffect $ Ref.write kept messages
                  Sockets.broadcast posted message
                  pure message
              , history: \_ -> liftEffect $ Ref.read messages
              , members: \_ -> nub <<< map _.tag <$> Sockets.connected sockets
              , typing: Sockets.broadcast typing
              }
          , alarm: mempty
          , connect: \socket -> Sockets.broadcast joined socket.tag
          , disconnect: \socket -> Sockets.broadcast left socket.tag
          }
