module Chat.Page.Session
  ( loadingView
  , failedView
  , lockedView
  , lobbyView
  , joiningView
  , handle
  ) where

import Prelude

import Chat.Session (RoomId)
import Chat.Session as Session
import Chat.Page.Browser (NotificationPermission(..), localStorage, location, notificationPermission, requestNotifications)
import Chat.Page.Room as Room
import Chat.Room (describeUserNameError, mkUserName, printUserName)
import Chat.Page.Types (App, Joining, Lobby, Locked, SessionAction(..), View(..))
import Chat.Style.Session (styles)
import Control.Monad.Error.Class (catchError)
import Data.Either (Either(..), either, isLeft)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (trim)
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.Button as Button
import UI.Core (Tone(..))
import UI.Field as Field
import UI.Status as Status
import UI.Style (css)
import Web.Event.Event (preventDefault)
import Web.HTML.Location as Location
import Web.Storage.Storage as Storage

primary :: Button.Options
primary = Button.defaults { tone = Accent, styles = styles.wide }

screen :: forall action w. HH.HTML w action -> HH.HTML w action
screen = HH.main [ css styles.screen ] <<< pure

loadingView :: forall w. HH.HTML w SessionAction
loadingView = screen $
  HH.div [ css styles.card ]
    [ HH.h1 [ css styles.title ] [ HH.text "Chat" ]
    , HH.p [ css styles.lead ] [ HH.text "Loading…" ]
    ]

failedView :: forall w. String -> HH.HTML w SessionAction
failedView why = screen $
  HH.div [ css styles.card ]
    [ HH.h1 [ css styles.title ] [ HH.text "Chat" ]
    , Status.error [ HH.text why ]
    , Button.button primary [ HE.onClick \_ -> Initialize ] [ HH.text "Try again" ]
    ]

lockedView :: forall w. Locked -> HH.HTML w SessionAction
lockedView { passkey, error, busy } = screen $
  HH.form [ css styles.card, HE.onSubmit Unlock ]
    [ HH.h1 [ css styles.title ] [ HH.text "Passkey" ]
    , HH.p [ css styles.lead ] [ HH.text "This chat is private. Enter the passkey to continue." ]
    , Field.password field
        [ HP.placeholder "Enter the passkey", HP.autofocus true, HP.value passkey, HE.onValueInput SetPasskey ]
    , Button.submit (primary { disabled = busy || trim passkey == "", busy = busy }) []
        [ HH.text if busy then "Checking…" else "Unlock" ]
    ]
  where
  field = (Field.defaults "chat-passkey" "Passkey") { error = error, required = true, disabled = busy }

lobbyView :: forall w. Lobby -> HH.HTML w SessionAction
lobbyView { busy, error } = screen $
  HH.div [ css styles.card ]
    [ HH.h1 [ css styles.title ] [ HH.text "Chat" ]
    , HH.p [ css styles.lead ] [ HH.text "Each room is one Durable Object. Its id is the link; anyone who has it can talk." ]
    , maybe (HH.text "") (\why -> Status.error [ HH.text why ]) error
    , Button.button (primary { busy = busy, disabled = busy }) [ HE.onClick \_ -> CreateRoom ]
        [ HH.text if busy then "Creating…" else "Create a room" ]
    ]

joiningView :: forall w. Joining -> HH.HTML w SessionAction
joiningView { name, error } = screen $
  HH.form [ css styles.card, HE.onSubmit SubmitName ]
    [ HH.h1 [ css styles.title ] [ HH.text "Who are you?" ]
    , HH.p [ css styles.lead ] [ HH.text "The name others will see in this room." ]
    , maybe (HH.text "") (\why -> Status.error [ HH.text why ]) error
    , Field.input ((Field.defaults "chat-name" "Your name") { required = true })
        [ HP.placeholder "Your name", HP.autofocus true, HP.value name, HE.onValueInput SetName ]
    , Button.submit (primary { disabled = isLeft $ mkUserName name }) [] [ HH.text "Join" ]
    ]

handle :: forall m. MonadAff m => SessionAction -> App m (Maybe RoomId)
handle = case _ of
  Initialize -> do
    H.modify_ _ { view = Loading }
    stored <- liftEffect $ fromMaybe "" <$> (Storage.getItem authorKey =<< localStorage)
    let author = either (const "") printUserName $ mkUserName stored
    notifications <- liftEffect notificationPermission
    H.modify_ _ { author = author, notifications = notifications }
    outcome <- liftAff $ attempt Session.sessionStatus
    case outcome of
      Left _ -> H.modify_ _ { view = LoadFailed "Could not check the session." } $> Nothing
      Right status | Session.admitted status -> roomFromUrl
      Right _ -> H.modify_ _ { view = Locked { passkey: "", error: Nothing, busy: false } } $> Nothing
  SetPasskey passkey ->
    modifyLocked (_ { passkey = passkey, error = Nothing }) $> Nothing
  Unlock event -> do
    liftEffect $ preventDefault event
    state <- H.get
    result <- case state.view of
      Locked locked -> do
        modifyLocked _ { busy = true }
        outcome <- liftAff $ attempt $ Session.loginStatus $ trim locked.passkey
        case outcome of
          Left _ -> modifyLocked (_ { busy = false, error = Just "Could not check the passkey." }) $> Nothing
          Right status | Session.admitted status -> H.modify_ _ { view = Lobby { busy: false, error: Nothing } } *> roomFromUrl
          Right 401 -> modifyLocked (_ { busy = false, error = Just "That passkey is not right." }) $> Nothing
          Right 403 -> modifyLocked (_ { busy = false, error = Just "That passkey is not right." }) $> Nothing
          Right _ -> modifyLocked (_ { busy = false, error = Just "The session service is not available." }) $> Nothing
      _ -> pure Nothing
    pure result
  CreateRoom -> do
    state <- H.get
    case state.view of
      Lobby lobby | not lobby.busy -> do
        modifyLobby _ { busy = true, error = Nothing }
        outcome <- liftAff $ attempt Session.create
        case outcome of
          Left _ -> modifyLobby (_ { busy = false, error = Just "Could not create a room." }) $> Nothing
          Right id -> pure $ Just id
      _ -> pure Nothing
  SetName name -> modifyJoining (_ { name = name, error = Nothing }) $> Nothing
  SubmitName event -> do
    liftEffect $ preventDefault event
    state <- H.get
    case state.view of
      Joining joining -> case mkUserName joining.name of
        Left why -> modifyJoining (_ { error = Just $ describeUserNameError why }) $> Nothing
        Right name -> do
          let author = printUserName name
          liftEffect $ Storage.setItem authorKey author =<< localStorage
          H.modify_ _ { author = author }
          pure $ Just joining.id
      _ -> pure Nothing
  ChangeName -> do
    author <- H.gets _.author
    Room.leaveRoom \room -> Joining { id: room.session.id, name: author, error: Nothing }
    pure Nothing
  EnableNotifications -> do
    outcome <- liftAff $ catchError requestNotifications (const $ pure Unsupported)
    H.modify_ _ { notifications = outcome }
    pure Nothing

roomFromUrl :: forall m. MonadAff m => App m (Maybe RoomId)
roomFromUrl = do
  fragment <- liftEffect $ Location.hash =<< location
  case Session.fromRoute fragment of
    Just id -> pure $ Just id
    Nothing -> H.modify_ _ { view = Lobby { busy: false, error: Nothing } } $> Nothing

modifyLocked :: forall m. (Locked -> Locked) -> App m Unit
modifyLocked update = H.modify_ \state -> state
  { view = case state.view of
      Locked locked -> Locked $ update locked
      view -> view
  }

modifyJoining :: forall m. (Joining -> Joining) -> App m Unit
modifyJoining update = H.modify_ \state -> state
  { view = case state.view of
      Joining joining -> Joining $ update joining
      view -> view
  }

modifyLobby :: forall m. (Lobby -> Lobby) -> App m Unit
modifyLobby update = H.modify_ \state -> state
  { view = case state.view of
      Lobby lobby -> Lobby $ update lobby
      view -> view
  }

authorKey :: String
authorKey = "chat.author"
