module Chat.Presentation
  ( main
  ) where

import Prelude

import Chat.Page.Browser (NotificationPermission(..), TimeFormatter, location)
import Chat.Page.Composer as Composer
import Chat.Page.Messages as Messages
import Chat.Page.Room as Room
import Chat.Page.Session as SessionView
import Chat.Page.Types (ComposerStatus(..), RoomToken(..), RoomView)
import Chat.Room (Author(..), ImageId(..), Message, MessageId(..), mkAuthor)
import Chat.Session (RoomSession)
import Chat.Session as Session
import Data.Array (take)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (SubscriptionId(..))
import Halogen.VDom.Driver (runUI)
import Web.HTML.Location as Location

data Fixture
  = SessionLoading
  | SessionFailed
  | SessionLocked
  | SessionLobby
  | SessionJoining
  | RoomEmpty
  | RoomConversation
  | RoomUploading
  | RoomSending

type State =
  { fixture :: Fixture
  , session :: RoomSession
  }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  fragment <- liftEffect $ Location.hash =<< location
  case Session.fromRoute "presentation-room" of
    Nothing -> pure unit
    Just id -> do
      let session = (Session.open id) { imageUrl = const "/chat-presentation.svg" }
      void $ runUI page { fixture: parseFixture fragment, session } body

page :: forall query m. MonadAff m => H.Component query State Void m
page = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. State -> H.ComponentHTML Unit () m
render state =
  HH.div
    [ HP.attr (HH.AttrName "data-presentation") (fixtureName state.fixture) ]
    [ fixtureView state ]

fixtureView :: forall w. State -> HH.HTML w Unit
fixtureView { fixture, session } = case fixture of
  SessionLoading -> map (const unit) SessionView.loadingView
  SessionFailed -> map (const unit) $ SessionView.failedView "Could not check the session."
  SessionLocked -> map (const unit) $ SessionView.lockedView { passkey: "wrong passkey", error: Just "That passkey is not right.", busy: false }
  SessionLobby -> map (const unit) $ SessionView.lobbyView { busy: false, error: Nothing }
  SessionJoining -> map (const unit) $ SessionView.joiningView { id: session.id, name: "Ada", error: Nothing }
  RoomEmpty -> roomView $ emptyRoom session
  RoomConversation -> roomView $ conversationRoom session
  RoomUploading -> roomView $ uploadingRoom session
  RoomSending -> roomView $ sendingRoom session

roomView :: forall w. RoomView -> HH.HTML w Unit
roomView room =
  Room.roomView
    { copyLink: unit
    , enableNotifications: unit
    , changeName: unit
    , leave: unit
    }
    { author: "Ada", notifications: Default }
    room
    ( Messages.messageList
        { react: \_ _ -> unit
        , jumpTo: const unit
        , reply: const unit
        }
        fixtureTime
        "Ada"
        room
    )
    (map (const unit) $ Composer.composer "Ada" room (Room.typingLine room))

fixtureTime :: TimeFormatter
fixtureTime milliseconds
  | milliseconds < 120000.0 = "10:40"
  | milliseconds < 240000.0 = "10:42"
  | milliseconds < 360000.0 = "10:44"
  | otherwise = "10:46"

baseRoom :: RoomSession -> RoomView
baseRoom session =
  { token: RoomToken 0
  , session
  , shareUrl: "https://chat.example/#presentation-room"
  , composer: { draft: "", replyTo: Nothing, attachments: [], status: Editing }
  , messages: Map.empty
  , feed: SubscriptionId 0
  , ticker: SubscriptionId 1
  , online: true
  , members: [ "Ada", "Grace", "Lin" ]
  , typing: Map.empty
  , typingSentAt: 0.0
  , unread: 0
  , error: Nothing
  , copied: false
  }

emptyRoom :: RoomSession -> RoomView
emptyRoom session = (baseRoom session)
  { online = false
  , members = [ "Ada" ]
  }

conversationRoom :: RoomSession -> RoomView
conversationRoom session = (baseRoom session)
  { composer =
      { draft: "@G"
      , replyTo: Just $ MessageId 1
      , attachments: [ ImageId 1 ]
      , status: Editing
      }
  , messages = messageMap conversationMessages
  , typing = Map.fromFoldable [ "Grace" /\ 1.0, "Lin" /\ 1.0 ]
  , copied = true
  }

uploadingRoom :: RoomSession -> RoomView
uploadingRoom session = (baseRoom session)
  { composer =
      { draft: ""
      , replyTo: Nothing
      , attachments: [ ImageId 1 ]
      , status: Uploading
      }
  , messages = messageMap $ take 2 conversationMessages
  }

sendingRoom :: RoomSession -> RoomView
sendingRoom session = (baseRoom session)
  { composer =
      { draft: "Sending this update now"
      , replyTo: Just $ MessageId 1
      , attachments: []
      , status: Sending
      }
  , messages = messageMap $ take 3 conversationMessages
  }

messageMap :: Array Message -> Map.Map MessageId Message
messageMap = Map.fromFoldable <<< map (\message -> message.id /\ message)

conversationMessages :: Array Message
conversationMessages =
  [ (fixtureMessage 1 "Grace" markdownSample 60000.0)
      { mentions = [ "Ada" ]
      , reactions =
          [ { emoji: "👍", by: [ "Ada", "Grace" ] }
          , { emoji: "🎉", by: [ "Ada" ] }
          ]
      }
  , fixtureMessage 2 "Grace" "One more detail in the same thread." 90000.0
  , (fixtureMessage 3 "Ada" "Thanks @Grace — I will take the next step." 180000.0)
      { replyTo = Just $ MessageId 1
      , mentions = [ "Grace" ]
      , reactions = [ { emoji: "❤️", by: [ "Grace" ] } ]
      }
  , (fixtureMessage 4 "ai" "Here is the image preview you asked for." 300000.0)
      { images = [ ImageId 1 ] }
  ]

fixtureMessage :: Int -> String -> String -> Number -> Message
fixtureMessage id name text sentAt =
  { id: MessageId id
  , author: fixtureAuthor name
  , text
  , images: []
  , replyTo: Nothing
  , mentions: []
  , reactions: []
  , sentAt
  }

fixtureAuthor :: String -> Author
fixtureAuthor name = case mkAuthor name of
  Right author -> author
  Left _ -> Assistant

markdownSample :: String
markdownSample = "# Launch notes\n\nHello @Ada and @ai. This has **bold text**, *italic text*, and `inline code`. Read the [room guide](https://example.com/chat).\n\n- Keep the link private\n- Reply in context\n\n> Ship the smallest clear change.\n\n```purescript\nmain = pure unit\n```"

parseFixture :: String -> Fixture
parseFixture = case _ of
  "#session-loading" -> SessionLoading
  "#session-failed" -> SessionFailed
  "#session-locked" -> SessionLocked
  "#session-lobby" -> SessionLobby
  "#session-joining" -> SessionJoining
  "#room-empty" -> RoomEmpty
  "#room-uploading" -> RoomUploading
  "#room-sending" -> RoomSending
  _ -> RoomConversation

fixtureName :: Fixture -> String
fixtureName = case _ of
  SessionLoading -> "session-loading"
  SessionFailed -> "session-failed"
  SessionLocked -> "session-locked"
  SessionLobby -> "session-lobby"
  SessionJoining -> "session-joining"
  RoomEmpty -> "room-empty"
  RoomConversation -> "room-conversation"
  RoomUploading -> "room-uploading"
  RoomSending -> "room-sending"
