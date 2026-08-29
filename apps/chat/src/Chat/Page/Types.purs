module Chat.Page.Types
  ( State
  , View(..)
  , RoomToken(..)
  , ComposerStatus(..)
  , ComposerState
  , RoomView
  , Locked
  , Lobby
  , Joining
  , Action(..)
  , SessionAction(..)
  , RoomAction(..)
  , ComposerAction(..)
  , App
  , Html
  , modifyRoom
  , modifyRoomAt
  , withRoom
  , advanceRoomToken
  ) where

import Prelude

import Chat.Client (RoomId)
import Chat.Page.Browser (NotificationPermission, TimeFormatter)
import Chat.Room (Message, RoomApi, RoomEvents)
import Cloudflare.Durable (Signal)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Halogen as H
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.Event.Event (Event)
import Web.File.File (File)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

type State =
  { author :: String
  , formatTime :: TimeFormatter
  , notifications :: NotificationPermission
  , nextRoomToken :: RoomToken
  , view :: View
  }

data View
  = Loading
  | LoadFailed String
  | Locked Locked
  | Lobby Lobby
  | Joining Joining
  | InRoom RoomView

newtype RoomToken = RoomToken Int

derive newtype instance Eq RoomToken

data ComposerStatus
  = Editing
  | Uploading
  | Sending

derive instance Eq ComposerStatus

type ComposerState =
  { draft :: String
  , replyTo :: Maybe Int
  , attachments :: Array Int
  , status :: ComposerStatus
  }

type Locked = { passkey :: String, error :: Maybe String, busy :: Boolean }

type Lobby = { busy :: Boolean, error :: Maybe String }

type Joining = { id :: RoomId, name :: String, error :: Maybe String }

type RoomView =
  { token :: RoomToken
  , id :: RoomId
  , api :: Record RoomApi
  , shareUrl :: String
  , composer :: ComposerState
  , messages :: Map Int Message
  , feed :: H.SubscriptionId
  , ticker :: H.SubscriptionId
  , online :: Boolean
  , members :: Array String
  , typing :: Map String Number
  , typingSentAt :: Number
  , unread :: Int
  , error :: Maybe String
  , copied :: Boolean
  }

data Action
  = Session SessionAction
  | Room RoomAction
  | Composer ComposerAction

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
  | Tick RoomToken
  | Notified RoomToken (Signal (Variant RoomEvents))

data ComposerAction
  = SetDraft String
  | KeyDown KeyboardEvent
  | Pasted ClipboardEvent
  | PickMention String
  | Attach
  | SelectedFiles (Array File)
  | Detach Int
  | Reply (Maybe Int)
  | Submit Event

type App m = H.HalogenM State Action () Void m

type Html m = H.ComponentHTML Action () m

modifyRoom :: forall m. (RoomView -> RoomView) -> App m Unit
modifyRoom update = H.modify_ \state -> state
  { view = case state.view of
      InRoom room -> InRoom $ update room
      view -> view
  }

modifyRoomAt :: forall m. RoomToken -> (RoomView -> RoomView) -> App m Unit
modifyRoomAt token update = H.modify_ \state -> state
  { view = case state.view of
      InRoom room | room.token == token -> InRoom $ update room
      view -> view
  }

withRoom :: forall m. (RoomView -> App m Unit) -> App m Unit
withRoom use = H.get >>= \state -> case state.view of
  InRoom room -> use room
  _ -> pure unit

advanceRoomToken :: RoomToken -> RoomToken
advanceRoomToken (RoomToken token) = RoomToken (token + 1)
