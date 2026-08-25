module Frontend.Chat
  ( main
  ) where

import Prelude

import Chat.Client (Chat, RoomId)
import Chat.Client as Chat
import Chat.Room (Message, RoomApi, RoomEvents)
import Cloudflare.Durable (Signal(..))
import Cloudflare.Durable.Rpc as Rpc
import Data.Variant (Variant, match)
import Data.Array (zip)
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Enum (fromEnum)
import Data.String as String
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (drop, null, trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.HTMLElement (HTMLElement, focus)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

foreign import formatTime :: Number -> String
foreign import nearBottom :: HTMLElement -> Effect Boolean
foreign import scrollToEnd :: HTMLElement -> Effect Unit
foreign import copyText :: String -> Effect Unit

chat :: Chat
chat = Chat.connect "/rpc"

type State =
  { author :: String
  , view :: View
  }

data View
  = Lobby { busy :: Boolean }
  | Joining { id :: RoomId, name :: String }
  | InRoom RoomView

type RoomView =
  { id :: RoomId
  , room :: Record RoomApi
  , link :: String
  , draft :: String
  , messages :: Map Int Message
  , feed :: H.SubscriptionId
  , online :: Boolean
  , members :: Array String
  , error :: Maybe String
  , sending :: Boolean
  , copied :: Boolean
  }

data Action
  = Initialize
  | CreateRoom
  | Enter RoomId
  | SetName String
  | SubmitName Event
  | Join RoomId
  | ChangeName
  | SetDraft String
  | Submit Event
  | Notified (Signal (Variant RoomEvents))
  | Loaded { messages :: Array Message, members :: Array String }
  | CopyLink
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
      }
  }

messagesRef :: H.RefLabel
messagesRef = H.RefLabel "messages"

type Html m = H.ComponentHTML Action () m

render :: forall m. State -> Html m
render st = case st.view of
  Lobby lobby -> lobbyView lobby
  Joining joining -> joiningView joining
  InRoom r -> roomView st.author r

lobbyView :: forall m. { busy :: Boolean } -> Html m
lobbyView { busy } =
  HH.main [ cls "centered" ]
    [ HH.div [ cls "card" ]
        [ HH.h1_ [ HH.text "Chat" ]
        , HH.p [ cls "hint" ]
            [ HH.text "Each room is one Durable Object. Its id is the link; anyone who has it can talk." ]
        , HH.button
            [ cls "primary", HP.disabled busy, HE.onClick \_ -> CreateRoom ]
            [ HH.text if busy then "Creating…" else "Create a room" ]
        ]
    ]

joiningView :: forall m. { id :: RoomId, name :: String } -> Html m
joiningView { name } =
  HH.main [ cls "centered" ]
    [ HH.form [ cls "card", HE.onSubmit SubmitName ]
        [ HH.h1_ [ HH.text "Who are you?" ]
        , HH.p [ cls "hint" ] [ HH.text "The name others will see in this room." ]
        , HH.input
            [ HP.placeholder "Your name"
            , HP.autofocus true
            , HP.value name
            , HE.onValueInput SetName
            ]
        , HH.button
            [ cls "primary", HP.type_ HP.ButtonSubmit, HP.disabled (null (trim name)) ]
            [ HH.text "Join" ]
        ]
    ]

roomView :: forall m. String -> RoomView -> Html m
roomView author r =
  HH.main [ cls "room" ]
    [ roomHeader author r
    , messageList author r
    , composer r
    ]

roomHeader :: forall m. String -> RoomView -> Html m
roomHeader author r =
  HH.header_
    [ HH.div [ cls "room-title" ]
        [ HH.span [ cls if r.online then "dot online" else "dot" ] []
        , HH.h1_ [ HH.text "Room" ]
        , HH.code [ cls "room-id", HP.title (Chat.printRoomId r.id) ] [ HH.text $ shortId r.id ]
        , HH.button [ cls "chip", HE.onClick \_ -> CopyLink ]
            [ linkIcon, HH.text if r.copied then "Copied" else "Copy link" ]
        ]
    , HH.div [ cls "room-actions" ]
        [ HH.div [ cls "members", HP.title (String.joinWith ", " r.members) ] (avatar <$> r.members)
        , HH.button [ cls "identity", HP.title "Change name", HE.onClick \_ -> ChangeName ]
            [ avatar author, HH.span_ [ HH.text author ] ]
        , HH.button [ cls "quiet", HE.onClick \_ -> Leave ] [ HH.text "Leave" ]
        ]
    ]

messageList :: forall m. String -> RoomView -> Html m
messageList author r =
  HH.ol [ cls "messages", HP.ref messagesRef ]
    if Map.isEmpty r.messages then [ emptyRoom ]
    else messageItem author <$> threaded (Array.fromFoldable r.messages)

emptyRoom :: forall m. Html m
emptyRoom =
  HH.li [ cls "empty" ]
    [ HH.p [ cls "empty-title" ] [ HH.text "It's quiet in here" ]
    , HH.p [ cls "hint" ] [ HH.text "Share the link and say hello." ]
    ]

-- | One bubble. `continued` means the same author just spoke, so the avatar
-- | and name are left out and the bubble tucks under the previous one.
messageItem :: forall m. String -> Tuple Boolean Message -> Html m
messageItem author (Tuple continued m) =
  HH.li [ HP.classes $ HH.ClassName <$> [ side ] <> (if continued then [ "continued" ] else []) ]
    [ if headless then HH.span [ cls "gutter" ] [] else avatar m.author
    , HH.div [ cls "bubble" ]
        [ if headless then HH.text "" else HH.span [ cls "author" ] [ HH.text m.author ]
        , HH.p_ [ HH.text m.text ]
        , HH.span [ cls "time" ] [ HH.text $ formatTime m.sentAt ]
        ]
    ]
  where
  mine = m.author == author
  side = if mine then "mine" else "theirs"
  headless = mine || continued

composer :: forall m. RoomView -> Html m
composer r =
  HH.footer_
    [ HH.form [ cls "composer", HE.onSubmit Submit ]
        [ HH.input
            [ HP.placeholder "Message"
            , HP.autofocus true
            , HP.autocomplete HP.AutocompleteOff
            , HP.value r.draft
            , HP.ref composerRef
            , HE.onValueInput SetDraft
            ]
        , HH.button
            [ cls "send", HP.type_ HP.ButtonSubmit, HP.title "Send", HP.disabled (r.sending || null (trim r.draft)) ]
            [ sendIcon ]
        ]
    , case r.error of
        Just message -> HH.p [ cls "error" ] [ HH.text message ]
        Nothing -> HH.text ""
    ]

avatar :: forall w i. String -> HH.HTML w i
avatar name = HH.span [ cls "avatar", HP.style ("--hue: " <> show (hue name)) ]
  [ HH.text $ String.toUpper $ String.take 1 name ]
  where
  hue = String.toCodePointArray >>> map fromEnum >>> Array.foldl (\h c -> (h * 31 + c) `mod` 360) 7

shortId :: RoomId -> String
shortId id = let s = Chat.printRoomId id in String.take 6 s <> "…" <> String.drop (String.length s - 4) s

linkIcon :: forall w i. HH.HTML w i
linkIcon = HH.elementNS svgNs (HH.ElemName "svg")
  [ HP.attr (HH.AttrName "viewBox") "0 0 24 24", HP.attr (HH.AttrName "aria-hidden") "true" ]
  [ path "M10 13a5 5 0 0 0 7.5.5l3-3a5 5 0 0 0-7-7l-1.7 1.7"
  , path "M14 11a5 5 0 0 0-7.5-.5l-3 3a5 5 0 0 0 7 7l1.7-1.7"
  ]

sendIcon :: forall w i. HH.HTML w i
sendIcon = HH.elementNS svgNs (HH.ElemName "svg")
  [ HP.attr (HH.AttrName "viewBox") "0 0 24 24", HP.attr (HH.AttrName "aria-hidden") "true" ]
  [ path "M12 19V5", path "m5 12 7-7 7 7" ]

path :: forall w i. String -> HH.HTML w i
path d = HH.elementNS svgNs (HH.ElemName "path") [ HP.attr (HH.AttrName "d") d ] []

svgNs :: HH.Namespace
svgNs = HH.Namespace "http://www.w3.org/2000/svg"

composerRef :: H.RefLabel
composerRef = H.RefLabel "composer"

cls :: forall r i. String -> HH.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

-- | Pair each message with whether it continues the previous author's run.
threaded :: Array Message -> Array (Tuple Boolean Message)
threaded messages = zip ([ false ] <> (continues <$> zip messages (Array.drop 1 messages))) messages
  where
  continues (Tuple previous next) = previous.author == next.author && next.sentAt - previous.sentAt < 300000.0

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    author <- liftEffect $ fromMaybe "" <$> (Storage.getItem authorKey =<< localStorage)
    H.modify_ _ { author = author }
    fragment <- liftEffect $ drop 1 <$> (Location.hash =<< location)
    case Chat.parseRoomId chat fragment of
      Just id -> handleAction $ Enter id
      Nothing -> pure unit

  CreateRoom -> do
    H.modify_ _ { view = Lobby { busy: true } }
    id <- liftAff $ Chat.create chat
    handleAction $ Enter id

  Enter id -> do
    liftEffect $ Location.setHash (Chat.printRoomId id) =<< location
    { author } <- H.get
    if null (trim author) then H.modify_ _ { view = Joining { id, name: "" } }
    else handleAction $ Join id

  SetName name -> H.modify_ \st -> case st.view of
    Joining j -> st { view = Joining j { name = name } }
    _ -> st

  SubmitName event -> do
    liftEffect $ preventDefault event
    st <- H.get
    case st.view of
      Joining { id, name } | not null (trim name) -> do
        liftEffect $ Storage.setItem authorKey (trim name) =<< localStorage
        H.modify_ _ { author = trim name }
        handleAction $ Join id
      _ -> pure unit

  Join id -> do
    link <- liftEffect $ Location.href =<< location
    { author } <- H.get
    let room = Chat.open chat id
    feed <- H.subscribe $ Notified <$> Chat.listen chat id author
    H.modify_ _
      { view = InRoom
          { id, room, link, draft: "", messages: Map.empty, feed, online: false, members: [], error: Nothing, sending: false, copied: false }
      }
    focusComposer

  ChangeName -> leaveRoom \r st -> st { view = Joining { id: r.id, name: st.author } }

  Notified signal -> case signal of
    Opened -> do
      inRoom _ { online = true, error = Nothing }
      reload
    Closed -> inRoom _ { online = false }
    Garbled why -> inRoom _ { error = Just $ "Unreadable event: " <> why }
    Delivered event -> event # match
      { message: \message -> do
          pinned <- withMessages nearBottom
          inRoom \r -> r { messages = Map.insert message.id message r.messages }
          { author } <- H.get
          when (fromMaybe true pinned || message.author == author) $ void $ withMessages scrollToEnd
      , joined: \_ -> reload
      , left: \_ -> reload
      }

  Loaded { messages, members } -> do
    -- Left-biased union: what we already hold wins over the reload.
    inRoom \r -> r { messages = Map.union r.messages (byId messages), members = members }
    void $ withMessages scrollToEnd

  SetDraft draft -> inRoom _ { draft = draft }

  CopyLink -> do
    st <- H.get
    case st.view of
      InRoom r -> do
        liftEffect $ copyText r.link
        inRoom _ { copied = true }
      _ -> pure unit

  Submit event -> do
    liftEffect $ preventDefault event
    st <- H.get
    case st.view of
      InRoom r | not null (trim r.draft) -> do
        inRoom _ { sending = true, error = Nothing }
        outcome <- liftAff $ Rpc.run $ r.room.post { author: st.author, text: r.draft }
        inRoom case outcome of
          Right _ -> _ { sending = false, draft = "" }
          Left failure -> _ { sending = false, error = Just $ Chat.describeFailure failure }
        focusComposer
      _ -> pure unit

  Leave -> do
    liftEffect $ Location.setHash "" =<< location
    leaveRoom \_ st -> st { view = Lobby { busy: false } }
  where
  inRoom f = H.modify_ \st -> case st.view of
    InRoom r -> st { view = InRoom (f r) }
    _ -> st

  -- History and members from the object; run on open and on presence changes.
  reload = do
    st <- H.get
    case st.view of
      InRoom r -> do
        outcome <- liftAff $ Rpc.run do
          messages <- Rpc.infallible $ r.room.history unit
          members <- r.room.members unit
          pure { messages, members }
        case outcome of
          Right loaded -> handleAction $ Loaded loaded
          Left failure -> inRoom _ { error = Just $ Chat.describeFailure failure }
      _ -> pure unit

  leaveRoom next = do
    st <- H.get
    case st.view of
      InRoom r -> do
        H.unsubscribe r.feed
        H.put $ next r st
      _ -> pure unit

  focusComposer = H.getHTMLElementRef composerRef >>= traverse_ (liftEffect <<< focus)

  location = Window.location =<< window
  localStorage = Window.localStorage =<< window
  authorKey = "chat.author"

withMessages
  :: forall a output m
   . MonadAff m
  => (HTMLElement -> Effect a)
  -> H.HalogenM State Action () output m (Maybe a)
withMessages act = H.getHTMLElementRef messagesRef >>= case _ of
  Just element -> Just <$> liftEffect (act element)
  Nothing -> pure Nothing

byId :: Array Message -> Map Int Message
byId = Map.fromFoldable <<< map \m -> Tuple m.id m
