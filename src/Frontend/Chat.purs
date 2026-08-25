module Frontend.Chat
  ( main
  ) where

import Prelude

import Chat.Client (Chat, RoomId)
import Chat.Client as Chat
import Chat.Markdown (Block(..), Inline(..))
import Chat.Markdown as Markdown
import Chat.Room (Message, RoomApi, RoomEvents)
import Chat.Room.Live (assistantName)
import Cloudflare.Durable (Signal(..))
import Cloudflare.Durable.Rpc as Rpc
import Control.Promise (Promise, toAffE)
import Data.Array (elem, filter, last, length, take, zip)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, joinWith, null, split, stripPrefix, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (attempt, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription (makeEmitter)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.HTML (window)
import Web.HTML.HTMLElement (HTMLElement, focus)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, toEvent)

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
foreign import scrollToId :: String -> Effect Unit
foreign import pickAndUpload :: String -> Effect (Promise (Array Int))
foreign import uploadPasted :: String -> Event -> Effect (Promise (Array Int))

chat :: Chat
chat = Chat.connect "/rpc"

-- State ------------------------------------------------------------------

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
  = Initialize
  | SetPasskey String
  | Unlock Event
  | CreateRoom
  | Enter RoomId
  | SetName String
  | SubmitName Event
  | Join RoomId
  | ChangeName
  | SetDraft String
  | KeyDown KeyboardEvent
  | Pasted Event
  | PickMention String
  | Attach
  | Attached (Array Int)
  | Detach Int
  | Reply (Maybe Int)
  | JumpTo Int
  | React Int String
  | Submit Event
  | Notified (Signal (Variant RoomEvents))
  | Loaded { messages :: Array Message, members :: Array String }
  | Tick
  | EnableNotifications
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
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

messagesRef :: H.RefLabel
messagesRef = H.RefLabel "messages"

composerRef :: H.RefLabel
composerRef = H.RefLabel "composer"

-- | Where the room serves and accepts images.
imageEndpoint :: RoomId -> String
imageEndpoint id = "/rpc/Room/id/" <> Chat.printRoomId id <> "/http/image"

imageUrl :: RoomId -> Int -> String
imageUrl id n = imageEndpoint id <> "/" <> show n

-- Views --------------------------------------------------------------------

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
        , HH.input [ HP.type_ HP.InputPassword, HP.placeholder "Passkey", HP.autofocus true, HP.value passkey, HE.onValueInput SetPasskey ]
        , HH.button [ cls "primary", HP.type_ HP.ButtonSubmit, HP.disabled (busy || null (trim passkey)) ]
            [ HH.text if busy then "Checking…" else "Unlock" ]
        , errorLine error
        ]
    ]

lobbyView :: forall m. { busy :: Boolean } -> Html m
lobbyView { busy } =
  HH.main [ cls "centered" ]
    [ HH.div [ cls "card" ]
        [ HH.h1_ [ HH.text "Chat" ]
        , HH.p [ cls "hint" ] [ HH.text "Each room is one Durable Object. Its id is the link; anyone who has it can talk." ]
        , HH.button [ cls "primary", HP.disabled busy, HE.onClick \_ -> CreateRoom ]
            [ HH.text if busy then "Creating…" else "Create a room" ]
        ]
    ]

joiningView :: forall m. { id :: RoomId, name :: String } -> Html m
joiningView { name } =
  HH.main [ cls "centered" ]
    [ HH.form [ cls "card", HE.onSubmit SubmitName ]
        [ HH.h1_ [ HH.text "Who are you?" ]
        , HH.p [ cls "hint" ] [ HH.text "The name others will see in this room." ]
        , HH.input [ HP.placeholder "Your name", HP.autofocus true, HP.value name, HE.onValueInput SetName ]
        , HH.button [ cls "primary", HP.type_ HP.ButtonSubmit, HP.disabled (null (trim name)) ] [ HH.text "Join" ]
        ]
    ]

roomView :: forall m. State -> RoomView -> Html m
roomView st r =
  HH.main [ cls "room" ]
    [ roomHeader st r
    , messageList st.author r
    , composer st.author r
    ]

roomHeader :: forall m. State -> RoomView -> Html m
roomHeader { author, notifications } r =
  HH.header_
    [ HH.div [ cls "room-title" ]
        [ HH.span [ cls if r.online then "dot online" else "dot" ] []
        , HH.h1_ [ HH.text "Room" ]
        , HH.code [ cls "room-id", HP.title (Chat.printRoomId r.id) ] [ HH.text $ shortId r.id ]
        , HH.span [ cls "hint online-count" ] [ HH.text $ onlineLabel r ]
        , HH.button [ cls "chip", HE.onClick \_ -> CopyLink ] [ linkIcon, HH.text if r.copied then "Copied" else "Copy link" ]
        , if notifications == "default" then HH.button [ cls "chip", HE.onClick \_ -> EnableNotifications ] [ bellIcon, HH.text "Notify me" ]
          else HH.text ""
        ]
    , HH.div [ cls "room-actions" ]
        [ HH.div [ cls "members", HP.title (joinWith ", " r.members) ] (avatar <$> r.members)
        , HH.button [ cls "identity", HP.title "Change name", HE.onClick \_ -> ChangeName ] [ avatar author, HH.span_ [ HH.text author ] ]
        , HH.button [ cls "quiet", HE.onClick \_ -> Leave ] [ HH.text "Leave" ]
        ]
    ]

onlineLabel :: RoomView -> String
onlineLabel r
  | not r.online = "connecting…"
  | otherwise = case length r.members of
      1 -> "just you"
      n -> show n <> " online"

messageList :: forall m. String -> RoomView -> Html m
messageList author r =
  HH.ol [ cls "messages", HP.ref messagesRef ]
    if Map.isEmpty r.messages then [ emptyRoom ]
    else messageItem author r <$> threaded (Array.fromFoldable r.messages)

emptyRoom :: forall m. Html m
emptyRoom =
  HH.li [ cls "empty" ]
    [ HH.p [ cls "empty-title" ] [ HH.text "It's quiet in here" ]
    , HH.p [ cls "hint" ] [ HH.text "Share the link and say hello." ]
    ]

-- | One bubble: quoted reply, markdown body, images, reactions, and a hover
-- | bar to react or reply. `continued` hides the avatar and name.
messageItem :: forall m. String -> RoomView -> Tuple Boolean Message -> Html m
messageItem author r (Tuple continued m) =
  HH.li
    [ HP.id ("msg-" <> show m.id)
    , HP.classes $ HH.ClassName <$>
        [ side ] <> flag continued "continued" <> flag bot "bot" <> flag mentioned "mentioned"
    ]
    [ if headless then HH.span [ cls "gutter" ] [] else avatar m.author
    , HH.div [ cls "stack" ]
        [ HH.div [ cls "bubble" ] $
            (if headless then [] else [ HH.span [ cls "author" ] [ HH.text m.author ] ])
              <> quoted
              <> markdown author m.text
              <> (m.images <#> \n -> HH.a [ HP.href (imageUrl r.id n), HP.target "_blank", cls "image" ] [ HH.img [ HP.src (imageUrl r.id n), HP.alt "attached image" ] ])
              <> [ HH.span [ cls "time" ] [ HH.text $ formatTime m.sentAt ] ]
        , reactions
        , HH.div [ cls "hover-bar" ] $
            (quickEmojis <#> \e -> HH.button [ cls "quiet", HP.title e, HE.onClick \_ -> React m.id e ] [ HH.text e ])
              <> [ HH.button [ cls "quiet", HP.title "Reply", HE.onClick \_ -> Reply (Just m.id) ] [ replyIcon ] ]
        ]
    ]
  where
  mine = m.author == author
  bot = m.author == assistantName
  mentioned = author `elem` m.mentions
  side = if mine then "mine" else "theirs"
  headless = mine || continued
  flag b c = if b then [ c ] else []

  quoted = case m.replyTo >>= \id -> Map.lookup id r.messages of
    Nothing -> []
    Just parent ->
      [ HH.button [ cls "quote", HE.onClick \_ -> JumpTo parent.id ]
          [ HH.span [ cls "quote-author" ] [ HH.text parent.author ]
          , HH.span [ cls "quote-text" ] [ HH.text $ String.take 120 parent.text ]
          ]
      ]

  reactions =
    if m.reactions == [] then HH.text ""
    else HH.div [ cls "reactions" ] $ m.reactions <#> \{ emoji, by } ->
      HH.button
        [ cls (if author `elem` by then "reaction mine" else "reaction"), HP.title (joinWith ", " by), HE.onClick \_ -> React m.id emoji ]
        [ HH.text emoji, HH.span [ cls "count" ] [ HH.text $ show (length by) ] ]

quickEmojis :: Array String
quickEmojis = [ "👍", "❤️", "😂", "🎉", "👀" ]

-- | Markdown to Halogen, with mentions of the reader marked.
markdown :: forall m. String -> String -> Array (Html m)
markdown me = map block <<< Markdown.parse
  where
  block = case _ of
    Paragraph xs -> HH.p_ (inline <$> xs)
    Heading 1 xs -> HH.h2_ (inline <$> xs)
    Heading 2 xs -> HH.h3_ (inline <$> xs)
    Heading _ xs -> HH.h4_ (inline <$> xs)
    Quote bs -> HH.blockquote_ (block <$> bs)
    Bullets items -> HH.ul_ (items <#> \xs -> HH.li_ (inline <$> xs))
    Code _ body -> HH.pre_ [ HH.code_ [ HH.text body ] ]
  inline = case _ of
    Text s -> HH.text s
    Bold xs -> HH.strong_ (inline <$> xs)
    Italic xs -> HH.em_ (inline <$> xs)
    InlineCode s -> HH.code_ [ HH.text s ]
    Link { text, url }
      | safe url -> HH.a [ HP.href url, HP.target "_blank", HP.rel "noopener noreferrer" ] [ HH.text text ]
      | otherwise -> HH.text text
    Mention name -> HH.span [ cls (if name == me then "mention me" else "mention") ] [ HH.text $ "@" <> name ]
  safe url = isJust (stripPrefix (Pattern "https://") url) || isJust (stripPrefix (Pattern "http://") url) || isJust (stripPrefix (Pattern "mailto:") url)

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

-- | Names that complete the `@word` the cursor is on, if any.
suggestions :: String -> RoomView -> Array String
suggestions author r = case last (split (Pattern " ") r.draft) >>= stripPrefix (Pattern "@") of
  Just partial | not (String.contains (Pattern "\n") partial) ->
    take 6 $ filter (\n -> n /= author && isJust (stripPrefix (Pattern (String.toLower partial)) (String.toLower n))) candidates
  _ -> []
  where
  candidates = Array.nub $ [ assistantName ] <> r.members <> (Array.fromFoldable r.messages <#> _.author)

composer :: forall m. String -> RoomView -> Html m
composer author r =
  HH.footer_
    [ typingLine r
    , case r.replyTo >>= \id -> Map.lookup id r.messages of
        Just parent ->
          HH.div [ cls "reply-chip" ]
            [ replyIcon
            , HH.span_ [ HH.text $ "Replying to ", HH.strong_ [ HH.text parent.author ], HH.text $ ": " <> String.take 60 parent.text ]
            , HH.button [ cls "quiet", HP.title "Cancel", HE.onClick \_ -> Reply Nothing ] [ HH.text "×" ]
            ]
        Nothing -> HH.text ""
    , if r.attachments == [] && not r.uploading then HH.text ""
      else HH.div [ cls "attachments" ] $
        (r.attachments <#> \n -> HH.span [ cls "attachment" ] [ HH.img [ HP.src (imageUrl r.id n) ], HH.button [ cls "quiet", HE.onClick \_ -> Detach n ] [ HH.text "×" ] ])
          <> (if r.uploading then [ HH.span [ cls "attachment uploading" ] [ HH.text "…" ] ] else [])
    , case suggestions author r of
        [] -> HH.text ""
        names -> HH.div [ cls "suggest" ] $ names <#> \n -> HH.button [ cls "quiet", HE.onClick \_ -> PickMention n ] [ avatar n, HH.text n ]
    , HH.form [ cls "composer", HE.onSubmit Submit ]
        [ HH.button [ cls "quiet attach", HP.type_ HP.ButtonButton, HP.title "Attach image", HE.onClick \_ -> Attach ] [ imageIcon ]
        , HH.input
            [ HP.placeholder $ "Message · @" <> assistantName <> " to ask the assistant · markdown ok"
            , HP.autofocus true
            , HP.autocomplete HP.AutocompleteOff
            , HP.value r.draft
            , HP.ref composerRef
            , HE.onValueInput SetDraft
            , HE.onKeyDown KeyDown
            , HE.handler (EventType "paste") Pasted
            ]
        , HH.button
            [ cls "send", HP.type_ HP.ButtonSubmit, HP.title "Send", HP.disabled (r.sending || r.uploading || (null (trim r.draft) && r.attachments == [])) ]
            [ sendIcon ]
        ]
    , errorLine r.error
    ]

errorLine :: forall m. Maybe String -> Html m
errorLine = case _ of
  Just why -> HH.p [ cls "error" ] [ HH.text why ]
  Nothing -> HH.text ""

avatar :: forall w i. String -> HH.HTML w i
avatar name = HH.span [ cls (if name == assistantName then "avatar bot" else "avatar"), HP.style ("--hue: " <> show (hue name)) ]
  [ HH.text if name == assistantName then "✦" else String.toUpper $ String.take 1 name ]
  where
  hue = String.toCodePointArray >>> map fromEnum >>> Array.foldl (\h c -> (h * 31 + c) `mod` 360) 7

shortId :: RoomId -> String
shortId id = let s = Chat.printRoomId id in String.take 6 s <> "…" <> String.drop (String.length s - 4) s

icon :: forall w i. Array String -> HH.HTML w i
icon paths = HH.elementNS svgNs (HH.ElemName "svg")
  [ HP.attr (HH.AttrName "viewBox") "0 0 24 24", HP.attr (HH.AttrName "aria-hidden") "true" ]
  (paths <#> \d -> HH.elementNS svgNs (HH.ElemName "path") [ HP.attr (HH.AttrName "d") d ] [])
  where
  svgNs = HH.Namespace "http://www.w3.org/2000/svg"

linkIcon :: forall w i. HH.HTML w i
linkIcon = icon [ "M10 13a5 5 0 0 0 7.5.5l3-3a5 5 0 0 0-7-7l-1.7 1.7", "M14 11a5 5 0 0 0-7.5-.5l-3 3a5 5 0 0 0 7 7l1.7-1.7" ]

bellIcon :: forall w i. HH.HTML w i
bellIcon = icon [ "M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9", "M10.3 21a1.94 1.94 0 0 0 3.4 0" ]

sendIcon :: forall w i. HH.HTML w i
sendIcon = icon [ "M12 19V5", "m5 12 7-7 7 7" ]

replyIcon :: forall w i. HH.HTML w i
replyIcon = icon [ "M9 17 4 12l5-5", "M20 18v-2a4 4 0 0 0-4-4H4" ]

imageIcon :: forall w i. HH.HTML w i
imageIcon = icon [ "M3 5h18v14H3z", "m3 15 5-5 4 4 3-3 6 6", "M16 8h.01" ]

cls :: forall r i. String -> HH.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

-- | Pair each message with whether it continues the previous author's run.
threaded :: Array Message -> Array (Tuple Boolean Message)
threaded messages = zip ([ false ] <> (continues <$> zip messages (Array.drop 1 messages))) messages
  where
  continues (Tuple previous next) = previous.author == next.author && next.sentAt - previous.sentAt < 300000.0 && next.replyTo == Nothing

-- Actions ------------------------------------------------------------------

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
        if outcome == 204 then H.modify_ _ { view = Lobby { busy: false } } *> enterFromUrl
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
          { id, room, link, draft: "", replyTo: Nothing, attachments: [], uploading: false, messages: Map.empty
          , feed, ticker, online: false, members: [], typing: Map.empty, typingSentAt: 0.0, unread: 0
          , error: Nothing, sending: false, copied: false
          }
      }
    focusComposer

  ChangeName -> leaveRoom \r st -> st { view = Joining { id: r.id, name: st.author } }

  Notified signal -> case signal of
    Opened -> inRoom _ { online = true, error = Nothing } *> reload
    Closed -> inRoom _ { online = false }
    Garbled why -> inRoom _ { error = Just $ "Unreadable event: " <> why }
    Delivered event -> event # match
      { message: \m -> do
          pinned <- withMessages nearBottom
          inRoom \r -> r { messages = Map.insert m.id m r.messages, typing = Map.delete m.author r.typing }
          { author } <- H.get
          when (fromMaybe true pinned || m.author == author) $ void $ withMessages scrollToEnd
          when (m.author /= author) $ announce m (author `elem` m.mentions)
      , updated: \m -> inRoom \r -> r { messages = Map.insert m.id m r.messages }
      , joined: \_ -> reload
      , left: \name -> inRoom (\r -> r { typing = Map.delete name r.typing }) *> reload
      , typing: \name -> do
          { author } <- H.get
          at <- liftEffect nowMs
          when (name /= author) $ inRoom \r -> r { typing = Map.insert name at r.typing }
      }

  Loaded { messages, members } -> do
    -- Left-biased union: what we already hold wins over the reload.
    inRoom \r -> r { messages = Map.union r.messages (byId messages), members = members }
    void $ withMessages scrollToEnd

  Tick -> do
    at <- liftEffect nowMs
    inRoom \r -> r { typing = Map.filter (\seen -> at - seen < typingTtl) r.typing }
    here <- liftEffect $ not <$> away
    when here $ inRoom _ { unread = 0 } *> liftEffect (setTitle "Chat")

  EnableNotifications -> do
    outcome <- liftAff $ toAffE requestNotifications
    H.modify_ _ { notifications = outcome }

  SetDraft draft -> do
    inRoom _ { draft = draft }
    st <- H.get
    at <- liftEffect nowMs
    case st.view of
      InRoom r | not null (trim draft), at - r.typingSentAt > typingThrottle -> do
        inRoom _ { typingSentAt = at }
        void $ liftAff $ Rpc.run $ r.room.typing st.author
      _ -> pure unit

  KeyDown event -> do
    st <- H.get
    case st.view, key event of
      InRoom r, "Tab" | Just first <- Array.head (suggestions st.author r) -> do
        liftEffect $ preventDefault $ toEvent event
        handleAction $ PickMention first
      InRoom _, "Escape" -> inRoom _ { replyTo = Nothing }
      _, _ -> pure unit

  PickMention name -> do
    inRoom \r -> r { draft = replaceLastWord ("@" <> name <> " ") r.draft }
    focusComposer

  Pasted event -> do
    st <- H.get
    case st.view of
      InRoom r -> uploadWith (uploadPasted (imageEndpoint r.id) event)
      _ -> pure unit

  Attach -> do
    st <- H.get
    case st.view of
      InRoom r -> uploadWith (pickAndUpload (imageEndpoint r.id))
      _ -> pure unit

  Attached ids -> inRoom \r -> r { attachments = r.attachments <> ids, uploading = false }

  Detach n -> inRoom \r -> r { attachments = filter (_ /= n) r.attachments }

  Reply target -> inRoom _ { replyTo = target } *> focusComposer

  JumpTo id -> liftEffect $ scrollToId $ "msg-" <> show id

  React id emoji -> do
    st <- H.get
    case st.view of
      InRoom r -> do
        outcome <- liftAff $ Rpc.run $ r.room.react { id, emoji, by: st.author }
        case outcome of
          Right m -> inRoom \v -> v { messages = Map.insert m.id m v.messages }
          Left failure -> inRoom _ { error = Just $ Chat.describeFailure failure }
      _ -> pure unit

  Submit event -> do
    liftEffect $ preventDefault event
    st <- H.get
    case st.view of
      InRoom r | not (null (trim r.draft) && r.attachments == []) -> do
        inRoom _ { sending = true, error = Nothing }
        outcome <- liftAff $ Rpc.run $ r.room.post { author: st.author, text: r.draft, images: r.attachments, replyTo: r.replyTo }
        inRoom case outcome of
          Right _ -> _ { sending = false, draft = "", attachments = [], replyTo = Nothing }
          Left failure -> _ { sending = false, error = Just $ Chat.describeFailure failure }
        focusComposer
      _ -> pure unit

  CopyLink -> do
    st <- H.get
    case st.view of
      InRoom r -> liftEffect (copyText r.link) *> inRoom _ { copied = true }
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

  -- Run an upload, then attach what came back.
  uploadWith :: Effect (Promise (Array Int)) -> H.HalogenM State Action () output m Unit
  uploadWith go = do
    inRoom _ { uploading = true, error = Nothing }
    outcome <- liftAff $ attempt $ toAffE go
    case outcome of
      Right ids -> handleAction $ Attached ids
      Left err -> inRoom _ { uploading = false, error = Just $ "Upload failed: " <> message err }

  nowMs = unwrap <<< unInstant <$> now

  -- A desktop notification and a tab-title count, only while the user is away;
  -- a mention notifies even when the tab is visible but unfocused.
  announce m mentioned = do
    gone <- liftEffect away
    when (gone || mentioned) do
      inRoom \r -> r { unread = r.unread + 1 }
      st <- H.get
      case st.view of
        InRoom r -> liftEffect do
          when gone $ setTitle $ "(" <> show r.unread <> ") Chat"
          when (st.notifications == "granted") $
            notify { title: (if mentioned then "@" <> st.author <> " · " else "") <> m.author, body: String.take 200 m.text, tag: "room-" <> Chat.printRoomId r.id }
        _ -> pure unit

  enterFromUrl = do
    fragment <- liftEffect $ drop 1 <$> (Location.hash =<< location)
    case Chat.parseRoomId chat fragment of
      Just id -> handleAction $ Enter id
      Nothing -> pure unit

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

-- | Swap the word the cursor is on (the last one) for `replacement`.
replaceLastWord :: String -> String -> String
replaceLastWord replacement draft =
  let
    words = split (Pattern " ") draft
  in
    joinWith " " (Array.dropEnd 1 words) <> (if length words > 1 then " " else "") <> replacement

-- | How long a `typing` event keeps someone in the indicator, and how often
-- | we send one while the draft changes.
typingTtl :: Number
typingTtl = 3500.0

typingThrottle :: Number
typingThrottle = 1500.0
