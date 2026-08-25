module Frontend.Chat
  ( main
  ) where

import Prelude

import Chat.Client (Chat, RoomId)
import Chat.Client as Chat
import Chat.Room (Message, RoomApi, RoomEvents)
import Chat.Room.Live (assistantName)
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
import Effect.Now (now)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Halogen.Subscription (makeEmitter)
import Control.Promise (Promise, toAffE)
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
foreign import interval :: Int -> (Unit -> Effect Unit) -> Effect (Effect Unit)
foreign import notificationPermission :: Effect String
foreign import requestNotifications :: Effect (Promise String)
foreign import away :: Effect Boolean
foreign import notify :: { title :: String, body :: String, tag :: String } -> Effect Unit
foreign import setTitle :: String -> Effect Unit
foreign import sessionStatus :: Effect (Promise Int)
foreign import login :: String -> Effect (Promise Int)

chat :: Chat
chat = Chat.connect "/rpc"

type State =
  { author :: String
  , notifications :: String -- "default" | "granted" | "denied" | "unsupported"
  , view :: View
  }

data View
  = Locked { passkey :: String, error :: Maybe String, busy :: Boolean }
  | Lobby { busy :: Boolean }
  | Joining { id :: RoomId, name :: String }
  | InRoom RoomView

type RoomView =
  { id :: RoomId
  , room :: Record RoomApi
  , link :: String
  , draft :: String
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
  | Tick
  | EnableNotifications
  | SetPasskey String
  | Unlock Event
  | CopyLink
  | Leave

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI page unit body

page :: forall query input output m. MonadAff m => H.Component query input output m
page = H.mkComponent
  { initialState: \_ -> { author: "", notifications: "default", view: Lobby { busy: false } }
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
  Locked locked -> lockedView locked
  Lobby lobby -> lobbyView lobby
  Joining joining -> joiningView joining
  InRoom r -> roomView st r

lockedView :: forall m. { passkey :: String, error :: Maybe String, busy :: Boolean } -> Html m
lockedView { passkey, error, busy } =
  HH.main [ cls "centered" ]
    [ HH.form [ cls "card", HE.onSubmit Unlock ]
        [ HH.h1_ [ HH.text "Passkey" ]
        , HH.p [ cls "hint" ] [ HH.text "This chat is private. Enter the passkey to continue." ]
        , HH.input
            [ HP.type_ HP.InputPassword
            , HP.placeholder "Passkey"
            , HP.autofocus true
            , HP.value passkey
            , HE.onValueInput SetPasskey
            ]
        , HH.button
            [ cls "primary", HP.type_ HP.ButtonSubmit, HP.disabled (busy || null (trim passkey)) ]
            [ HH.text if busy then "Checking…" else "Unlock" ]
        , case error of
            Just why -> HH.p [ cls "error" ] [ HH.text why ]
            Nothing -> HH.text ""
        ]
    ]

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

roomView :: forall m. State -> RoomView -> Html m
roomView st r =
  HH.main [ cls "room" ]
    [ roomHeader st r
    , messageList st.author r
    , composer r
    ]

roomHeader :: forall m. State -> RoomView -> Html m
roomHeader { author, notifications } r =
  HH.header_
    [ HH.div [ cls "room-title" ]
        [ HH.span [ cls if r.online then "dot online" else "dot" ] []
        , HH.h1_ [ HH.text "Room" ]
        , HH.code [ cls "room-id", HP.title (Chat.printRoomId r.id) ] [ HH.text $ shortId r.id ]
        , HH.span [ cls "hint online-count" ] [ HH.text $ onlineLabel r ]
        , HH.button [ cls "chip", HE.onClick \_ -> CopyLink ]
            [ linkIcon, HH.text if r.copied then "Copied" else "Copy link" ]
        , if notifications == "default" then
            HH.button [ cls "chip", HE.onClick \_ -> EnableNotifications ] [ bellIcon, HH.text "Notify me" ]
          else HH.text ""
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
  HH.li [ HP.classes $ HH.ClassName <$> [ side ] <> (if continued then [ "continued" ] else []) <> (if bot then [ "bot" ] else []) ]
    [ if headless then HH.span [ cls "gutter" ] [] else avatar m.author
    , HH.div [ cls "bubble" ]
        [ if headless then HH.text "" else HH.span [ cls "author" ] [ HH.text m.author ]
        , HH.p_ [ HH.text m.text ]
        , HH.span [ cls "time" ] [ HH.text $ formatTime m.sentAt ]
        ]
    ]
  where
  mine = m.author == author
  bot = m.author == assistantName
  side = if mine then "mine" else "theirs"
  headless = mine || continued

onlineLabel :: RoomView -> String
onlineLabel r
  | not r.online = "connecting…"
  | otherwise = case Array.length r.members of
      1 -> "just you"
      n -> show n <> " online"

-- | "ann is typing", "ann and bob are typing", or "several people are typing".
typingLine :: forall m. RoomView -> Html m
typingLine r =
  HH.div [ cls if Map.isEmpty r.typing then "typing" else "typing visible" ]
    [ HH.span [ cls "dots" ] [ HH.i_ [], HH.i_ [], HH.i_ [] ]
    , HH.span_ [ HH.text $ who $ Array.fromFoldable $ Map.keys r.typing ]
    ]
  where
  who = case _ of
    [] -> ""
    [ a ] -> a <> " is typing"
    [ a, b ] -> a <> " and " <> b <> " are typing"
    _ -> "several people are typing"

composer :: forall m. RoomView -> Html m
composer r =
  HH.footer_
    [ typingLine r
    , HH.form [ cls "composer", HE.onSubmit Submit ]
        [ HH.input
            [ HP.placeholder $ "Message · @" <> assistantName <> " to ask the assistant"
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
avatar name = HH.span [ cls (if name == assistantName then "avatar bot" else "avatar"), HP.style ("--hue: " <> show (hue name)) ]
  [ HH.text if name == assistantName then "✦" else String.toUpper $ String.take 1 name ]
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

bellIcon :: forall w i. HH.HTML w i
bellIcon = HH.elementNS svgNs (HH.ElemName "svg")
  [ HP.attr (HH.AttrName "viewBox") "0 0 24 24", HP.attr (HH.AttrName "aria-hidden") "true" ]
  [ path "M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9", path "M10.3 21a1.94 1.94 0 0 0 3.4 0" ]

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
    notifications <- liftEffect notificationPermission
    H.modify_ _ { author = author, notifications = notifications }
    admitted <- liftAff $ toAffE sessionStatus
    if admitted == 204 then enterFromUrl
    else H.modify_ _ { view = Locked { passkey: "", error: Nothing, busy: false } }

  SetPasskey passkey -> H.modify_ \st -> case st.view of
    Locked l -> st { view = Locked l { passkey = passkey, error = Nothing } }
    _ -> st

  Unlock event -> do
    liftEffect $ preventDefault event
    st <- H.get
    case st.view of
      Locked l -> do
        H.modify_ _ { view = Locked l { busy = true } }
        outcome <- liftAff $ toAffE $ login (trim l.passkey)
        if outcome == 204 then do
          H.modify_ _ { view = Lobby { busy: false } }
          enterFromUrl
        else H.modify_ _ { view = Locked l { busy = false, error = Just "That passkey is not right." } }
      _ -> pure unit

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
    ticker <- H.subscribe $ Tick <$ makeEmitter (interval 1000)
    H.modify_ _
      { view = InRoom
          { id
          , room
          , link
          , draft: ""
          , messages: Map.empty
          , feed
          , ticker
          , online: false
          , members: []
          , typing: Map.empty
          , typingSentAt: 0.0
          , error: Nothing
          , unread: 0
          , sending: false
          , copied: false
          }
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
          inRoom \r -> r { messages = Map.insert message.id message r.messages, typing = Map.delete message.author r.typing }
          { author } <- H.get
          when (fromMaybe true pinned || message.author == author) $ void $ withMessages scrollToEnd
          when (message.author /= author) $ announce message
      , joined: \_ -> reload
      , left: \name -> do
          inRoom \r -> r { typing = Map.delete name r.typing }
          reload
      , typing: \name -> do
          { author } <- H.get
          at <- liftEffect nowMs
          when (name /= author) $ inRoom \r -> r { typing = Map.insert name at r.typing }
      }

  -- Forget anyone who has not typed for a few seconds; clear unread once seen.
  Tick -> do
    at <- liftEffect nowMs
    inRoom \r -> r { typing = Map.filter (\seen -> at - seen < typingTtl) r.typing }
    here <- liftEffect $ not <$> away
    when here $ inRoom _ { unread = 0 } *> liftEffect (setTitle "Chat")

  EnableNotifications -> do
    outcome <- liftAff $ toAffE requestNotifications
    H.modify_ _ { notifications = outcome }

  Loaded { messages, members } -> do
    -- Left-biased union: what we already hold wins over the reload.
    inRoom \r -> r { messages = Map.union r.messages (byId messages), members = members }
    void $ withMessages scrollToEnd

  SetDraft draft -> do
    inRoom _ { draft = draft }
    st <- H.get
    at <- liftEffect nowMs
    case st.view of
      InRoom r | not null (trim draft), at - r.typingSentAt > typingThrottle -> do
        inRoom _ { typingSentAt = at }
        void $ liftAff $ Rpc.run $ r.room.typing st.author
      _ -> pure unit

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
        H.unsubscribe r.ticker
        H.put $ next r st
      _ -> pure unit

  focusComposer = H.getHTMLElementRef composerRef >>= traverse_ (liftEffect <<< focus)

  nowMs = unwrap <<< unInstant <$> now

  enterFromUrl = do
    fragment <- liftEffect $ drop 1 <$> (Location.hash =<< location)
    case Chat.parseRoomId chat fragment of
      Just id -> handleAction $ Enter id
      Nothing -> pure unit

  -- A desktop notification and a tab-title count, only while the user is away.
  announce message = do
    gone <- liftEffect away
    when gone do
      inRoom \r -> r { unread = r.unread + 1 }
      st <- H.get
      case st.view of
        InRoom r -> liftEffect do
          setTitle $ "(" <> show r.unread <> ") Chat"
          when (st.notifications == "granted") $
            notify { title: message.author, body: message.text, tag: "room-" <> Chat.printRoomId r.id }
        _ -> pure unit

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

-- | How long a `typing` event keeps someone in the indicator, and how often
-- | we send one while the draft changes.
typingTtl :: Number
typingTtl = 3500.0

typingThrottle :: Number
typingThrottle = 1500.0
