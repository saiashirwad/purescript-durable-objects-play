module Chat.Page.Session
  ( lockedView
  , lobbyView
  , joiningView
  , handle
  ) where

import Prelude

import Chat.Client (Chat, RoomId)
import Chat.Client as Chat
import Chat.Page.Browser (localStorage, location, notificationPermission, requestNotifications)
import Chat.Page.Types (App, Joining, Locked, SessionAction(..), View(..), _Joining, _Locked, _view, withRoom)
import Chat.Style (styles)
import Control.Promise (toAffE)
import Data.Argonaut.Core as J
import Data.Lens (over, preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (drop, null, trim)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Fetch (Method(..), fetch)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.Button as Button
import UI.Core (Tone(..))
import UI.Field as Field
import UI.Style (css)
import Web.Event.Event (preventDefault)
import Web.HTML.Location as Location
import Web.Storage.Storage as Storage

-- | 204 when this browser already holds a session cookie.
sessionStatus :: Aff Int
sessionStatus = _.status <$> fetch "/session" {}

-- | 204 when the passkey is right.
login :: String -> Aff Int
login passkey = _.status <$> fetch "/login"
  { method: POST
  , headers: { "content-type": "application/json" }
  , body: J.stringify $ J.fromObject $ Object.singleton "passkey" $ J.fromString passkey
  }

chat :: Chat
chat = Chat.connect "/rpc"

primary :: Button.Options
primary = Button.defaults { tone = Accent, styles = styles.wide }

screen :: forall action w. HH.HTML w action -> HH.HTML w action
screen = HH.main [ css styles.screen ] <<< pure

lockedView :: forall w. Locked -> HH.HTML w SessionAction
lockedView { passkey, error, busy } = screen $
  HH.form [ css styles.card, HE.onSubmit Unlock ]
    [ HH.h1 [ css styles.title ] [ HH.text "Passkey" ]
    , HH.p [ css styles.lead ] [ HH.text "This chat is private. Enter the passkey to continue." ]
    , Field.password field
        [ HP.placeholder "Enter the passkey", HP.autofocus true, HP.value passkey, HE.onValueInput SetPasskey ]
    , Button.submit (primary { disabled = busy || blank passkey, busy = busy }) []
        [ HH.text if busy then "Checking…" else "Unlock" ]
    ]
  where
  field = (Field.defaults "chat-passkey" "Passkey") { error = error, required = true, disabled = busy }

lobbyView :: forall w. { busy :: Boolean } -> HH.HTML w SessionAction
lobbyView { busy } = screen $
  HH.div [ css styles.card ]
    [ HH.h1 [ css styles.title ] [ HH.text "Chat" ]
    , HH.p [ css styles.lead ] [ HH.text "Each room is one Durable Object. Its id is the link; anyone who has it can talk." ]
    , Button.button (primary { busy = busy }) [ HE.onClick \_ -> CreateRoom ]
        [ HH.text if busy then "Creating…" else "Create a room" ]
    ]

joiningView :: forall w. Joining -> HH.HTML w SessionAction
joiningView { name } = screen $
  HH.form [ css styles.card, HE.onSubmit SubmitName ]
    [ HH.h1 [ css styles.title ] [ HH.text "Who are you?" ]
    , HH.p [ css styles.lead ] [ HH.text "The name others will see in this room." ]
    , Field.input ((Field.defaults "chat-name" "Your name") { required = true })
        [ HP.placeholder "Your name", HP.autofocus true, HP.value name, HE.onValueInput SetName ]
    , Button.submit (primary { disabled = blank name }) [] [ HH.text "Join" ]
    ]

handle :: forall m. MonadAff m => SessionAction -> App m (Maybe RoomId)
handle = case _ of
  Initialize -> do
    author <- liftEffect $ fromMaybe "" <$> (Storage.getItem authorKey =<< localStorage)
    notifications <- liftEffect notificationPermission
    H.modify_ _ { author = author, notifications = notifications }
    admitted <- liftAff sessionStatus
    if admitted == 204 then roomFromUrl
    else H.modify_ _ { view = Locked { passkey: "", error: Nothing, busy: false } } $> Nothing
  SetPasskey passkey ->
    H.modify_ (over (_view <<< _Locked) _ { passkey = passkey, error = Nothing }) $> Nothing
  Unlock event -> do
    liftEffect $ preventDefault event
    result <- H.gets (preview (_view <<< _Locked)) >>= case _ of
      Nothing -> pure Nothing
      Just locked -> do
        H.modify_ $ over (_view <<< _Locked) _ { busy = true }
        admitted <- liftAff $ login (trim locked.passkey)
        if admitted == 204 then H.modify_ _ { view = Lobby { busy: false } } *> roomFromUrl
        else H.modify_ (over (_view <<< _Locked) _ { busy = false, error = Just "That passkey is not right." }) $> Nothing
    pure result
  CreateRoom -> do
    H.modify_ _ { view = Lobby { busy: true } }
    Just <$> liftAff (Chat.create chat)
  SetName name -> H.modify_ (over (_view <<< _Joining) _ { name = name }) $> Nothing
  SubmitName event -> do
    liftEffect $ preventDefault event
    H.gets (preview (_view <<< _Joining)) >>= case _ of
      Just { id, name } | not (blank name) -> do
        liftEffect $ Storage.setItem authorKey (trim name) =<< localStorage
        H.modify_ _ { author = trim name }
        pure $ Just id
      _ -> pure Nothing
  ChangeName -> do
    withRoom \room -> do
      H.unsubscribe room.feed
      H.unsubscribe room.ticker
      H.modify_ \state -> state { view = Joining { id: room.id, name: state.author } }
    pure Nothing
  EnableNotifications -> do
    outcome <- liftAff $ toAffE requestNotifications
    H.modify_ _ { notifications = outcome }
    pure Nothing

roomFromUrl :: forall m. MonadAff m => App m (Maybe RoomId)
roomFromUrl = do
  fragment <- liftEffect $ drop 1 <$> (Location.hash =<< location)
  pure $ Chat.parseRoomId chat fragment

blank :: String -> Boolean
blank = null <<< trim

authorKey :: String
authorKey = "chat.author"
