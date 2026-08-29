module Chat.Page.Types
  ( State
  , View(..)
  , RoomView
  , Locked
  , Joining
  , Action(..)
  , SessionAction(..)
  , RoomAction(..)
  , ComposerAction(..)
  , App
  , Html
  , inRoom
  , withRoom
  , _view
  , _Locked
  , _Joining
  , _InRoom
  ) where

import Prelude

import Chat.Client (RoomId)
import Chat.Room (Message, RoomApi, RoomEvents)
import Cloudflare.Durable (Signal)
import Data.Foldable (traverse_)
import Data.Lens (Lens', Prism', over, preview, prism')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Halogen as H
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type State =
  { author :: String
  , notifications :: String
  , view :: View
  }

data View
  = Locked Locked
  | Lobby { busy :: Boolean }
  | Joining Joining
  | InRoom RoomView

type Locked = { passkey :: String, error :: Maybe String, busy :: Boolean }

type Joining = { id :: RoomId, name :: String }

type RoomView =
  { id :: RoomId
  , room :: Record RoomApi
  , link :: String
  , draft :: String
  , replyTo :: Maybe Int
  , attachments :: Array Int
  , uploading :: Boolean
  , messages :: Map Int Message
  , feed :: H.SubscriptionId
  , ticker :: H.SubscriptionId
  , online :: Boolean
  , members :: Array String
  , typing :: Map String Number
  , typingSentAt :: Number
  , unread :: Int
  , error :: Maybe String
  , sending :: Boolean
  , copied :: Boolean
  }

data Action
  = Session SessionAction
  | Room RoomAction
  | Composer ComposerAction
  | Tick
  | Notified (Signal (Variant RoomEvents))

data SessionAction
  = Initialize
  | SetPasskey String
  | Unlock Event
  | CreateRoom
  | SetName String
  | SubmitName Event
  | ChangeName
  | EnableNotifications

data RoomAction
  = CopyLink
  | Leave
  | JumpTo Int
  | React Int String

data ComposerAction
  = SetDraft String
  | KeyDown KeyboardEvent
  | Pasted Event
  | PickMention String
  | Attach
  | Attached (Array Int)
  | Detach Int
  | Reply (Maybe Int)
  | Submit Event

type App m = H.HalogenM State Action () Void m

type Html m = H.ComponentHTML Action () m

_view :: Lens' State View
_view = prop (Proxy :: Proxy "view")

_Locked :: Prism' View Locked
_Locked = prism' Locked case _ of
  Locked l -> Just l
  _ -> Nothing

_Joining :: Prism' View Joining
_Joining = prism' Joining case _ of
  Joining j -> Just j
  _ -> Nothing

_InRoom :: Prism' View RoomView
_InRoom = prism' InRoom case _ of
  InRoom r -> Just r
  _ -> Nothing

inRoom :: forall m. (RoomView -> RoomView) -> App m Unit
inRoom = H.modify_ <<< over (_view <<< _InRoom)

withRoom :: forall m. (RoomView -> App m Unit) -> App m Unit
withRoom k = H.gets (preview (_view <<< _InRoom)) >>= traverse_ k
