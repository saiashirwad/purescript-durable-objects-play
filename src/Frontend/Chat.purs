module Frontend.Chat
  ( main
  ) where

import Prelude

import Chat.Client (Chat, Room, Subscription)
import Chat.Client as Chat
import Chat.Room (Message)
import Data.Array (reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (drop, null, trim)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

foreign import formatTime :: Number -> String

chat :: Chat
chat = Chat.connect "/rpc"

type State =
  { author :: String
  , view :: View
  }

data View
  = Lobby { busy :: Boolean }
  | Joining { room :: Room, name :: String }
  | InRoom RoomView

type RoomView =
  { room :: Room
  , link :: String
  , draft :: String
  , messages :: Array Message
  , subscription :: Subscription
  , error :: Maybe String
  , sending :: Boolean
  }

data Action
  = Initialize
  | CreateRoom
  | Enter Room
  | SetName String
  | SubmitName Event
  | Join Room
  | ChangeName
  | SetDraft String
  | Submit Event
  | Received (Array Message)
  | Leave

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI page unit body

page :: forall query input output m. MonadAff m => H.Component query input output m
page = H.mkComponent
  { initialState: \_ -> { author: "", view: Lobby { busy: false } }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Leave
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st = case st.view of
  Lobby lobby ->
    HH.main [ HP.class_ (HH.ClassName "centered") ]
      [ HH.h1_ [ HH.text "Chat" ]
      , HH.p [ HP.class_ (HH.ClassName "hint") ]
          [ HH.text "Each room is one Durable Object. Its id is the link; anyone who has it can talk." ]
      , HH.button
          [ HP.disabled lobby.busy, HE.onClick \_ -> CreateRoom ]
          [ HH.text if lobby.busy then "Creating…" else "Create a room" ]
      ]

  Joining joining ->
    HH.main [ HP.class_ (HH.ClassName "centered") ]
      [ HH.h1_ [ HH.text "Who are you?" ]
      , HH.p [ HP.class_ (HH.ClassName "hint") ] [ HH.text "Pick the name others will see in this room." ]
      , HH.form [ HE.onSubmit SubmitName ]
          [ HH.input
              [ HP.placeholder "Your name"
              , HP.autofocus true
              , HP.value joining.name
              , HE.onValueInput SetName
              ]
          , HH.button
              [ HP.type_ HP.ButtonSubmit, HP.disabled (null (trim joining.name)) ]
              [ HH.text "Join" ]
          ]
      ]

  InRoom r ->
    HH.main [ HP.class_ (HH.ClassName "room") ]
      [ HH.header_
          [ HH.div_
              [ HH.h1_ [ HH.text "Chat" ]
              , HH.p [ HP.class_ (HH.ClassName "hint") ]
                  [ HH.text $ "as " <> st.author <> " "
                  , HH.button [ HP.class_ (HH.ClassName "quiet"), HE.onClick \_ -> ChangeName ] [ HH.text "change" ]
                  ]
              ]
          , HH.label_
              [ HH.text "Invite link"
              , HH.input [ HP.readOnly true, HP.value r.link ]
              ]
          , HH.button [ HP.class_ (HH.ClassName "quiet"), HE.onClick \_ -> Leave ] [ HH.text "leave" ]
          ]
      , HH.ol [ HP.class_ (HH.ClassName "messages") ] $ reverse r.messages <#> \m ->
          HH.li [ HP.class_ (HH.ClassName if m.author == st.author then "mine" else "theirs") ]
            [ HH.span [ HP.class_ (HH.ClassName "author") ] [ HH.text m.author ]
            , HH.span [ HP.class_ (HH.ClassName "time") ] [ HH.text $ formatTime m.sentAt ]
            , HH.p_ [ HH.text m.text ]
            ]
      , HH.form [ HE.onSubmit Submit ]
          [ HH.input
              [ HP.placeholder "Say something"
              , HP.autofocus true
              , HP.value r.draft
              , HP.disabled r.sending
              , HE.onValueInput SetDraft
              ]
          , HH.button [ HP.type_ HP.ButtonSubmit, HP.disabled r.sending ] [ HH.text "send" ]
          ]
      , case r.error of
          Just message -> HH.p [ HP.class_ (HH.ClassName "error") ] [ HH.text message ]
          Nothing -> HH.text ""
      ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    author <- liftEffect $ fromMaybe "" <$> (Storage.getItem authorKey =<< localStorage)
    H.modify_ _ { author = author }
    fragment <- liftEffect $ drop 1 <$> (Location.hash =<< location)
    case Chat.parseRoomId chat fragment of
      Just id -> handleAction $ Enter $ Chat.openRoom chat id
      Nothing -> pure unit

  CreateRoom -> do
    H.modify_ _ { view = Lobby { busy: true } }
    created <- liftAff $ Chat.createRoom chat
    handleAction $ Enter $ Chat.openRoom chat created

  Enter room -> do
    liftEffect $ Location.setHash (Chat.printRoomId $ Chat.roomId room) =<< location
    { author } <- H.get
    if null (trim author) then H.modify_ _ { view = Joining { room, name: "" } }
    else handleAction $ Join room

  SetName name -> H.modify_ \st -> case st.view of
    Joining j -> st { view = Joining j { name = name } }
    _ -> st

  SubmitName event -> do
    liftEffect $ preventDefault event
    st <- H.get
    case st.view of
      Joining { room, name } | not null (trim name) -> do
        liftEffect $ Storage.setItem authorKey (trim name) =<< localStorage
        H.modify_ _ { author = trim name }
        handleAction $ Join room
      _ -> pure unit

  Join room -> do
    link <- liftEffect $ Location.href =<< location
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    subscription <- liftAff $ Chat.listen room 0 (HS.notify listener <<< Received)
    H.modify_ _
      { view = InRoom { room, link, draft: "", messages: [], subscription, error: Nothing, sending: false } }

  ChangeName -> do
    st <- H.get
    case st.view of
      InRoom r -> do
        liftAff $ Chat.stop r.subscription
        H.modify_ _ { view = Joining { room: r.room, name: st.author } }
      _ -> pure unit

  Received messages -> inRoom \r -> r { messages = r.messages <> messages }

  SetDraft draft -> inRoom _ { draft = draft }

  Submit event -> do
    liftEffect $ preventDefault event
    st <- H.get
    case st.view of
      InRoom r | not null (trim r.draft) -> do
        inRoom _ { sending = true, error = Nothing }
        outcome <- liftAff $ Chat.send r.room { author: st.author, text: r.draft }
        inRoom case outcome of
          Right _ -> _ { sending = false, draft = "" }
          Left failure -> _ { sending = false, error = Just $ Chat.describeFailure failure }
      _ -> pure unit

  Leave -> do
    st <- H.get
    case st.view of
      InRoom r -> liftAff $ Chat.stop r.subscription
      _ -> pure unit
    liftEffect $ Location.setHash "" =<< location
    H.modify_ _ { view = Lobby { busy: false } }
  where
  inRoom f = H.modify_ \st -> case st.view of
    InRoom r -> st { view = InRoom (f r) }
    _ -> st

  location = Window.location =<< window
  localStorage = Window.localStorage =<< window
  authorKey = "chat.author"
