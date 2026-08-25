module Chat.Room.Live
  ( roomLive
  ) where

import Prelude

import Chat.Room (Message, PostError(..), RoomApi, maxTextLength, room)
import Cloudflare.Durable (Live)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (Rpc, fail)
import Cloudflare.Durable.Storage as Storage
import Control.Parallel (parOneOf)
import Data.Array (filter, last, null, snoc, takeEnd)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (length, trim)
import Data.Time.Duration (Milliseconds(..))
import Effect.AVar as AVar
import Effect.Aff (delay)
import Effect.Aff.AVar as AffVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref

messagesKey :: Storage.Key (Array Message)
messagesKey = Storage.key "messages"

keptMessages :: Int
keptMessages = 500

pollTimeout :: Milliseconds
pollTimeout = Milliseconds 20000.0

roomLive :: Live "Room" RoomApi
roomLive =
  Durable.implement room ado
    state <- Durable.state
    in
      do
        stored <- Storage.get state messagesKey
        messages <- liftEffect $ Ref.new $ fromMaybe [] stored
        waiters <- liftEffect $ Ref.new Map.empty
        nextWaiter <- liftEffect $ Ref.new 0
        let
          current :: forall e. Rpc e (Array Message)
          current = liftEffect $ Ref.read messages

          newer n = filter \m -> m.id > n

          wake :: forall e. Rpc e Unit
          wake = liftEffect do
            waiting <- Ref.read waiters
            Ref.write Map.empty waiters
            for_ waiting \signal -> AVar.tryPut unit signal

          awaitPost :: forall e. Rpc e Unit
          awaitPost = do
            signal <- liftAff AffVar.empty
            key <- liftEffect $ Ref.modify (_ + 1) nextWaiter
            liftEffect $ Ref.modify_ (Map.insert key signal) waiters
            liftAff $ parOneOf [ AffVar.take signal, delay pollTimeout ]
            liftEffect $ Ref.modify_ (Map.delete key) waiters

        pure
          { post: \new -> do
              let author = trim new.author
              let text = trim new.text
              when (author == "") $ fail AuthorRequired
              when (text == "") $ fail TextRequired
              when (length text > maxTextLength) $ fail TextTooLong
              sentAt <- liftEffect $ unwrap <<< unInstant <$> now
              all <- current
              let message = { id: maybe 1 (\m -> m.id + 1) (last all), author, text, sentAt }
              let kept = takeEnd keptMessages $ snoc all message
              Storage.put state messagesKey kept
              liftEffect $ Ref.write kept messages
              wake
              pure message

          , history: \_ -> current

          , since: \n -> do
              fresh <- newer n <$> current
              if null fresh then do
                awaitPost
                newer n <$> current
              else pure fresh
          }
