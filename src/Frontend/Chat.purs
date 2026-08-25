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
import Data.Array (elem, filter, last, length, mapWithIndex, replicate, take, zip)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, traverse_)
import Data.Lens (Lens', Prism', over, preview, prism')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, joinWith, null, split, stripPrefix, trim)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (attempt, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Frontend.Chat.Style (styles)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Subscription (makeEmitter)
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import UI.Avatar as Avatar
import UI.Button as Button
import UI.Core (Size(..), Tone(..), dataAttr)
import UI.Field as Field
import UI.Icon as Icon
import UI.Input as Input
import UI.Status as Status
import UI.Style (Style, css)
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

-- | Optics into the state. A `Lens'` reaches a field, a `Prism'` one case
-- | of `View`; composed with `<<<` they focus one screen's state, so an
-- | action for that screen is `over` it and a read is `preview`.
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

page :: forall query input m. MonadAff m => H.Component query input Void m
page = H.mkComponent
  { initialState: \_ -> { author: "", notifications: "default", view: Lobby { busy: false } }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

-- | The room scrolls as a whole, so this sits on `main`.
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

-- Button presets.

small :: Button.Options
small = Button.defaults { size = Small }

quiet :: Button.Options
quiet = small { tone = Quiet }

primary :: Button.Options
primary = Button.defaults { tone = Accent, styles = styles.wide }

-- | One card, centered.
screen :: forall m. Html m -> Html m
screen = HH.main [ css styles.screen ] <<< pure

lockedView :: forall m. Locked -> Html m
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

lobbyView :: forall m. { busy :: Boolean } -> Html m
lobbyView { busy } = screen $
  HH.div [ css styles.card ]
    [ HH.h1 [ css styles.title ] [ HH.text "Chat" ]
    , HH.p [ css styles.lead ] [ HH.text "Each room is one Durable Object. Its id is the link; anyone who has it can talk." ]
    , Button.button (primary { busy = busy }) [ HE.onClick \_ -> CreateRoom ]
        [ HH.text if busy then "Creating…" else "Create a room" ]
    ]

joiningView :: forall m. Joining -> Html m
joiningView { name } = screen $
  HH.form [ css styles.card, HE.onSubmit SubmitName ]
    [ HH.h1 [ css styles.title ] [ HH.text "Who are you?" ]
    , HH.p [ css styles.lead ] [ HH.text "The name others will see in this room." ]
    , Field.input ((Field.defaults "chat-name" "Your name") { required = true })
        [ HP.placeholder "Your name", HP.autofocus true, HP.value name, HE.onValueInput SetName ]
    , Button.submit (primary { disabled = blank name }) [] [ HH.text "Join" ]
    ]

roomView :: forall m. State -> RoomView -> Html m
roomView st r =
  HH.main [ css styles.room, HP.ref messagesRef ]
    [ HH.header [ css styles.header ]
        [ roomTitle st r
        , roomPeople st r
        ]
    , messageList st.author r
    , composer st.author r
    ]

-- | Presence, the room's name and short id, and the two share buttons.
roomTitle :: forall m. State -> RoomView -> Html m
roomTitle { notifications } r =
  HH.div [ css styles.headerGroup ] $
    [ HH.span [ css $ styles.presence <> guard r.online styles.online, ARIA.hidden "true" ] []
    , HH.h1 [ css styles.roomName ] [ HH.text "Room" ]
    , HH.code [ css styles.roomId, HP.title (Chat.printRoomId r.id) ] [ HH.text $ shortId r.id ]
    , HH.span [ css styles.count, ARIA.role "status", ARIA.live "polite" ] [ HH.text $ onlineLabel r ]
    , Button.button small [ HE.onClick \_ -> CopyLink ]
        [ Icon.render linkIcon, HH.text if r.copied then "Copied" else "Copy link" ]
    ] <> guard (notifications == "default")
      [ Button.button small [ HE.onClick \_ -> EnableNotifications ]
          [ Icon.render bellIcon, HH.text "Notify me" ]
      ]

-- | Who is here, who you are, and the way out.
roomPeople :: forall m. State -> RoomView -> Html m
roomPeople { author } r =
  HH.div [ css styles.headerGroup ]
    [ HH.div [ css styles.members, ARIA.label $ "People online: " <> joinWith ", " r.members ] $
        r.members # mapWithIndex \i -> avatar $ styles.member <> guard (i > 0) styles.overlap
    , Button.button (quiet { styles = styles.identity }) [ HP.title "Change name", HE.onClick \_ -> ChangeName ]
        [ avatar mempty author, HH.span_ [ HH.text author ] ]
    , Button.button quiet [ HE.onClick \_ -> Leave ] [ HH.text "Leave" ]
    ]

onlineLabel :: RoomView -> String
onlineLabel r
  | not r.online = "connecting…"
  | otherwise = case length r.members of
      1 -> "just you"
      n -> show n <> " online"

messageList :: forall m. String -> RoomView -> Html m
messageList author r =
  HH.ol [ css styles.list, ARIA.label "Messages" ]
    if Map.isEmpty r.messages then [ emptyRoom ]
    else messageItem author r <$> threaded (Array.fromFoldable r.messages)

emptyRoom :: forall m. Html m
emptyRoom =
  HH.li [ css $ styles.message <> styles.empty ]
    [ HH.p [ css styles.emptyTitle ] [ HH.text "It's quiet in here" ]
    , HH.p [ css styles.muted ] [ HH.text "Share the link and say hello." ]
    ]

-- | One message: avatar, bubble, reactions, and a bar that shows on hover.
-- | `continued` means the same author just spoke, so no avatar or name.
messageItem :: forall m. String -> RoomView -> Tuple Boolean Message -> Html m
messageItem author r (Tuple continued m) =
  HH.li
    [ HP.id $ "msg-" <> show m.id
    , css $ styles.message <> guard mine styles.mine <> guard continued styles.continued
    , dataAttr "ui" "message"
    ]
    $ guard (not mine) [ if continued then HH.span [ css styles.gutter, ARIA.hidden "true" ] [] else avatar (guard bot styles.botAvatar) m.author ]
        <> [ HH.div [ css styles.stack ] [ bubble, reactions, actions ] ]
  where
  mine = m.author == author
  bot = m.author == assistantName
  mentioned = author `elem` m.mentions
  headless = mine || continued

  bubble = HH.div [ css bubbleStyle ] $ fold
    [ guard (not headless) [ HH.span [ css styles.author ] [ HH.text m.author ] ]
    , foldMap (pure <<< quote) $ m.replyTo >>= \id -> Map.lookup id r.messages
    , markdown author mine m.text
    , image <$> m.images
    , [ HH.span [ css styles.time ] [ HH.text $ formatTime m.sentAt ] ]
    ]

  bubbleStyle = fold
    [ styles.bubble
    , if mine then styles.mineBubble else styles.theirsBubble
    , guard (continued && mine) styles.mineJoined
    , guard (continued && not mine) styles.theirsJoined
    , guard bot styles.botBubble
    , guard mentioned styles.mentionedBubble
    ]

  quote parent =
    Button.button (quiet { styles = styles.quote <> guard mine styles.mineQuote }) [ HE.onClick \_ -> JumpTo parent.id ]
      [ HH.span [ css styles.quoteAuthor ] [ HH.text parent.author ]
      , HH.span [ css $ styles.quoteText <> guard mine styles.mineQuoteText ]
          [ HH.text $ String.take 120 $ Markdown.plain parent.text ]
      ]

  image n =
    HH.a [ css styles.imageLink, HP.href (imageUrl r.id n), HP.target "_blank", HP.rel "noopener noreferrer" ]
      [ HH.img [ css styles.image, HP.src (imageUrl r.id n), HP.alt $ m.author <> " attached an image" ] ]

  reactions
    | Array.null m.reactions = HH.text ""
    | otherwise = HH.div [ css $ styles.reactions <> guard mine styles.mineReactions ] $ m.reactions <#> \{ emoji, by } ->
        Button.button (quiet { styles = styles.reaction <> guard (author `elem` by) styles.activeReaction })
          [ HP.title (joinWith ", " by), HE.onClick \_ -> React m.id emoji ]
          [ HH.text emoji, HH.span [ css styles.reactionCount ] [ HH.text $ show (length by) ] ]

  actions =
    HH.div [ css $ styles.actions <> guard mine styles.mineActions, ARIA.label $ "Actions for " <> m.author <> "'s message", dataAttr "ui" "actions" ] $
      (quickEmojis <#> \emoji -> Button.iconButton ("React with " <> emoji) tiny [ HP.title emoji, HE.onClick \_ -> React m.id emoji ] [ HH.text emoji ])
        <> [ Button.iconButton "Reply" tiny [ HP.title "Reply", HE.onClick \_ -> Reply (Just m.id) ] [ Icon.render replyIcon ] ]
  tiny = quiet { styles = styles.actionButton }

quickEmojis :: Array String
quickEmojis = [ "👍", "❤️", "😂", "🎉", "👀" ]

-- | Markdown to Halogen, with mentions of the reader marked.
markdown :: forall m. String -> Boolean -> String -> Array (Html m)
markdown me mine = map block <<< Markdown.parse
  where
  block = case _ of
    Paragraph xs -> HH.p [ css styles.paragraph ] (inline <$> xs)
    Heading 1 xs -> HH.h2 [ css styles.subheading ] (inline <$> xs)
    Heading 2 xs -> HH.h3 [ css styles.subheading ] (inline <$> xs)
    Heading _ xs -> HH.h4 [ css styles.subheading ] (inline <$> xs)
    Quote bs -> HH.blockquote [ css styles.blockquote ] (block <$> bs)
    Bullets items -> HH.ul [ css styles.bullets ] (items <#> HH.li_ <<< map inline)
    Code _ body -> HH.pre [ css styles.codeBlock ] [ HH.code_ [ HH.text body ] ]
  inline = case _ of
    Text s -> HH.text s
    Bold xs -> HH.strong_ (inline <$> xs)
    Italic xs -> HH.em_ (inline <$> xs)
    InlineCode s -> HH.code [ css styles.inlineCode ] [ HH.text s ]
    Link { text, url }
      | safe url -> HH.a [ css styles.link, HP.href url, HP.target "_blank", HP.rel "noopener noreferrer" ] [ HH.text text ]
      | otherwise -> HH.text text
    Mention name ->
      HH.span [ css $ styles.mention <> guard mine styles.mineMention <> guard (name == me) styles.selfMention ]
        [ HH.text $ "@" <> name ]
  safe url = Array.any (\scheme -> isJust $ stripPrefix (Pattern scheme) url) [ "https://", "http://", "mailto:" ]

-- | "ann is typing", "ann and bob are typing", or "several people are typing".
typingLine :: forall m. RoomView -> Html m
typingLine r =
  HH.div
    [ css $ styles.typing <> guard (not $ Map.isEmpty r.typing) styles.typingVisible
    , ARIA.role "status"
    , ARIA.live "polite"
    , ARIA.atomic "true"
    ]
    [ HH.span [ css styles.dots, ARIA.hidden "true", dataAttr "ui" "dots" ] $ replicate 3 $ HH.i [ css styles.dot ] []
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
  HH.footer [ css styles.footer ]
    [ typingLine r
    , replyChip r
    , attachmentStrip r
    , suggestionBar author r
    , HH.form [ css styles.composer, HE.onSubmit Submit, dataAttr "ui" "composer" ]
        [ Button.iconButton "Attach image" (Button.defaults { tone = Quiet }) [ HP.title "Attach image", HE.onClick \_ -> Attach ]
            [ Icon.styled styles.largeIcon imageIcon ]
        , Input.text (Input.defaults { disabled = r.sending, styles = styles.input })
            [ HP.placeholder $ "Message · @" <> assistantName <> " to ask the assistant · markdown ok"
            , ARIA.label "Message"
            , HP.autofocus true
            , HP.autocomplete HP.AutocompleteOff
            , HP.value r.draft
            , HP.ref composerRef
            , HE.onValueInput SetDraft
            , HE.onKeyDown KeyDown
            , HE.handler (EventType "paste") Pasted
            ]
        , Button.submit
            (Button.defaults { tone = Accent, disabled = r.sending || r.uploading || not (sendable r), busy = r.sending, styles = styles.send })
            [ HP.title "Send", ARIA.label "Send" ]
            [ Icon.render sendIcon ]
        ]
    , errorLine r.error
    ]

sendable :: RoomView -> Boolean
sendable r = not (blank r.draft) || not (Array.null r.attachments)

suggestionBar :: forall m. String -> RoomView -> Html m
suggestionBar author r = case suggestions author r of
  [] -> HH.text ""
  names -> HH.div [ css styles.suggestions, ARIA.label "Mention suggestions" ] $ names <#> \name ->
    Button.button (quiet { styles = styles.suggestion }) [ HE.onClick \_ -> PickMention name ]
      [ avatar styles.compactAvatar name, HH.text name ]

replyChip :: forall m. RoomView -> Html m
replyChip r = case r.replyTo >>= \id -> Map.lookup id r.messages of
  Nothing -> HH.text ""
  Just parent ->
    HH.div [ css styles.replyChip ]
      [ Icon.render replyIcon
      , HH.span [ css styles.replyChipText ]
          [ HH.text "Replying to ", HH.strong_ [ HH.text parent.author ], HH.text $ ": " <> String.take 60 (Markdown.plain parent.text) ]
      , Button.iconButton "Cancel reply" quiet [ HP.title "Cancel", HE.onClick \_ -> Reply Nothing ] [ HH.text "×" ]
      ]

attachmentStrip :: forall m. RoomView -> Html m
attachmentStrip r
  | Array.null r.attachments && not r.uploading = HH.text ""
  | otherwise =
      HH.div [ css styles.attachments, ARIA.label "Image attachments" ] $
        (thumbnail <$> r.attachments)
          <> guard r.uploading [ HH.span [ css styles.uploading, ARIA.role "status" ] [ HH.text "Uploading an image…" ] ]
      where
      thumbnail n = HH.span [ css styles.attachment ]
        [ HH.img [ css styles.thumbnail, HP.src (imageUrl r.id n), HP.alt "Image attachment preview" ]
        , Button.iconButton ("Remove attachment " <> show n) (small { styles = styles.remove }) [ HE.onClick \_ -> Detach n ] [ HH.text "×" ]
        ]

errorLine :: forall m. Maybe String -> Html m
errorLine = maybe (HH.text "") \why -> Status.error [ HH.text why ]

avatar :: forall w i. Style -> String -> HH.HTML w i
avatar extra name = Avatar.avatar
  { fallback: if name == assistantName then "✦" else String.toUpper $ String.take 1 name
  , hue: Avatar.hue name
  , styles: extra
  }

shortId :: RoomId -> String
shortId id = String.take 6 s <> "…" <> String.drop (String.length s - 4) s
  where
  s = Chat.printRoomId id

blank :: String -> Boolean
blank = null <<< trim

-- | Pair each message with whether it continues the previous author's run.
threaded :: Array Message -> Array (Tuple Boolean Message)
threaded messages = zip ([ false ] <> (continues <$> zip messages (Array.drop 1 messages))) messages
  where
  continues (Tuple previous next) = previous.author == next.author && next.sentAt - previous.sentAt < 300000.0 && next.replyTo == Nothing

-- Icons: paths on a 24×24 grid.

linkIcon :: Icon.Icon
linkIcon = Icon.icon [ "M10 13a5 5 0 0 0 7.5.5l3-3a5 5 0 0 0-7-7l-1.7 1.7", "M14 11a5 5 0 0 0-7.5-.5l-3 3a5 5 0 0 0 7 7l1.7-1.7" ]

bellIcon :: Icon.Icon
bellIcon = Icon.icon [ "M6 8a6 6 0 0 1 12 0c0 7 3 9 3 9H3s3-2 3-9", "M10.3 21a1.94 1.94 0 0 0 3.4 0" ]

sendIcon :: Icon.Icon
sendIcon = Icon.icon [ "M12 19V5", "m5 12 7-7 7 7" ]

replyIcon :: Icon.Icon
replyIcon = Icon.icon [ "M9 17 4 12l5-5", "M20 18v-2a4 4 0 0 0-4-4H4" ]

imageIcon :: Icon.Icon
imageIcon = Icon.icon [ "M3 5h18v14H3z", "m3 15 5-5 4 4 3-3 6 6", "M16 8h.01" ]

-- Actions ------------------------------------------------------------------

type App m = H.HalogenM State Action () Void m

handleAction :: forall m. MonadAff m => Action -> App m Unit
handleAction = case _ of
  -- Session
  Initialize -> do
    author <- liftEffect $ fromMaybe "" <$> (Storage.getItem authorKey =<< localStorage)
    notifications <- liftEffect notificationPermission
    H.modify_ _ { author = author, notifications = notifications }
    admitted <- liftAff $ toAffE sessionStatus
    if admitted == 204 then enterFromUrl
    else H.modify_ _ { view = Locked { passkey: "", error: Nothing, busy: false } }
  SetPasskey passkey -> H.modify_ $ over (_view <<< _Locked) _ { passkey = passkey, error = Nothing }
  Unlock event -> do
    liftEffect $ preventDefault event
    H.gets (preview (_view <<< _Locked)) >>= traverse_ \l -> do
      H.modify_ $ over (_view <<< _Locked) _ { busy = true }
      admitted <- liftAff $ toAffE $ login (trim l.passkey)
      if admitted == 204 then H.modify_ _ { view = Lobby { busy: false } } *> enterFromUrl
      else H.modify_ $ over (_view <<< _Locked) _ { busy = false, error = Just "That passkey is not right." }

  -- Rooms
  CreateRoom -> do
    H.modify_ _ { view = Lobby { busy: true } }
    liftAff (Chat.create chat) >>= handleAction <<< Enter
  Enter id -> do
    liftEffect $ Location.setHash (Chat.printRoomId id) =<< location
    { author } <- H.get
    if blank author then H.modify_ _ { view = Joining { id, name: "" } }
    else handleAction $ Join id
  SetName name -> H.modify_ $ over (_view <<< _Joining) _ { name = name }
  SubmitName event -> do
    liftEffect $ preventDefault event
    H.gets (preview (_view <<< _Joining)) >>= traverse_ \{ id, name } -> unless (blank name) do
      liftEffect $ Storage.setItem authorKey (trim name) =<< localStorage
      H.modify_ _ { author = trim name }
      handleAction $ Join id
  Join id -> enterRoom id
  ChangeName -> leaveRoom \r st -> st { view = Joining { id: r.id, name: st.author } }
  Leave -> do
    liftEffect $ Location.setHash "" =<< location
    leaveRoom \_ st -> st { view = Lobby { busy: false } }
  CopyLink -> withRoom \r -> liftEffect (copyText r.link) *> inRoom _ { copied = true }

  -- The feed
  Notified signal -> onSignal signal
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

  -- The composer
  SetDraft draft -> inRoom _ { draft = draft } *> pingTyping
  KeyDown event -> withRoom \r -> do
    { author } <- H.get
    case key event, Array.head (suggestions author r) of
      "Tab", Just first -> do
        liftEffect $ preventDefault $ toEvent event
        handleAction $ PickMention first
      "Escape", _ -> inRoom _ { replyTo = Nothing }
      _, _ -> pure unit
  PickMention name -> do
    inRoom \r -> r { draft = replaceLastWord ("@" <> name <> " ") r.draft }
    focusComposer
  Pasted event -> withRoom \r -> upload $ uploadPasted (imageEndpoint r.id) event
  Attach -> withRoom \r -> upload $ pickAndUpload (imageEndpoint r.id)
  Attached ids -> inRoom \r -> r { attachments = r.attachments <> ids, uploading = false }
  Detach n -> inRoom \r -> r { attachments = filter (_ /= n) r.attachments }
  Reply target -> inRoom _ { replyTo = target } *> focusComposer
  JumpTo id -> liftEffect $ scrollToId $ "msg-" <> show id
  React id emoji -> withRoom \r -> do
    { author } <- H.get
    liftAff (Rpc.run $ r.room.react { id, emoji, by: author }) >>= either
      (\failure -> inRoom _ { error = Just $ Chat.describeFailure failure })
      (\m -> inRoom \v -> v { messages = Map.insert m.id m v.messages })
  Submit event -> liftEffect (preventDefault event) *> submit

-- | The state of the current room, if we are in one.
inRoom :: forall m. (RoomView -> RoomView) -> App m Unit
inRoom = H.modify_ <<< over (_view <<< _InRoom)

withRoom :: forall m. MonadAff m => (RoomView -> App m Unit) -> App m Unit
withRoom k = H.gets (preview (_view <<< _InRoom)) >>= traverse_ k

enterRoom :: forall m. MonadAff m => RoomId -> App m Unit
enterRoom id = do
  link <- liftEffect $ Location.href =<< location
  { author } <- H.get
  feed <- H.subscribe $ Notified <$> Chat.listen chat id author
  ticker <- H.subscribe $ Tick <$ makeEmitter (interval 1000)
  H.modify_ _
    { view = InRoom
        { id
        , room: Chat.open chat id
        , link
        , draft: ""
        , replyTo: Nothing
        , attachments: []
        , uploading: false
        , messages: Map.empty
        , feed
        , ticker
        , online: false
        , members: []
        , typing: Map.empty
        , typingSentAt: 0.0
        , unread: 0
        , error: Nothing
        , sending: false
        , copied: false
        }
    }
  focusComposer

leaveRoom :: forall m. MonadAff m => (RoomView -> State -> State) -> App m Unit
leaveRoom next = withRoom \r -> do
  H.unsubscribe r.feed
  H.unsubscribe r.ticker
  H.modify_ $ next r

onSignal :: forall m. MonadAff m => Signal (Variant RoomEvents) -> App m Unit
onSignal = case _ of
  Opened -> inRoom _ { online = true, error = Nothing } *> reload
  Closed -> inRoom _ { online = false }
  Garbled why -> inRoom _ { error = Just $ "Unreadable event: " <> why }
  Delivered event -> event # match
    { message: onMessage
    , updated: \m -> inRoom \r -> r { messages = Map.insert m.id m r.messages }
    , joined: \_ -> reload
    , left: \name -> inRoom (\r -> r { typing = Map.delete name r.typing }) *> reload
    , typing: \name -> do
        { author } <- H.get
        at <- liftEffect nowMs
        when (name /= author) $ inRoom \r -> r { typing = Map.insert name at r.typing }
    }

-- | Show a new message; stay pinned to the bottom if we were; announce
-- | others' messages.
onMessage :: forall m. MonadAff m => Message -> App m Unit
onMessage m = do
  pinned <- withMessages nearBottom
  inRoom \r -> r { messages = Map.insert m.id m r.messages, typing = Map.delete m.author r.typing }
  { author } <- H.get
  when (fromMaybe true pinned || m.author == author) $ void $ withMessages scrollToEnd
  when (m.author /= author) $ announce m (author `elem` m.mentions)

-- | A desktop notification and a tab-title count, only while the user is away;
-- | a mention notifies even when the tab is visible but unfocused.
announce :: forall m. MonadAff m => Message -> Boolean -> App m Unit
announce m mentioned = do
  gone <- liftEffect away
  when (gone || mentioned) do
    inRoom \r -> r { unread = r.unread + 1 }
    st <- H.get
    withRoom \r -> liftEffect do
      when gone $ setTitle $ "(" <> show r.unread <> ") Chat"
      when (st.notifications == "granted") $ notify
        { title: (if mentioned then "@" <> st.author <> " · " else "") <> m.author
        , body: String.take 200 (Markdown.plain m.text)
        , tag: "room-" <> Chat.printRoomId r.id
        }

-- | History and members from the object; run on open and on presence changes.
reload :: forall m. MonadAff m => App m Unit
reload = withRoom \r -> do
  outcome <- liftAff $ Rpc.run do
    messages <- Rpc.infallible $ r.room.history unit
    members <- r.room.members unit
    pure { messages, members }
  either (\failure -> inRoom _ { error = Just $ Chat.describeFailure failure }) (handleAction <<< Loaded) outcome

submit :: forall m. MonadAff m => App m Unit
submit = withRoom \r -> when (sendable r) do
  { author } <- H.get
  inRoom _ { sending = true, error = Nothing }
  outcome <- liftAff $ Rpc.run $ r.room.post { author, text: r.draft, images: r.attachments, replyTo: r.replyTo }
  inRoom case outcome of
    Right _ -> _ { sending = false, draft = "", attachments = [], replyTo = Nothing }
    Left failure -> _ { sending = false, error = Just $ Chat.describeFailure failure }
  focusComposer

-- | Tell the room we are typing, at most once per `typingThrottle`.
pingTyping :: forall m. MonadAff m => App m Unit
pingTyping = withRoom \r -> unless (blank r.draft) do
  at <- liftEffect nowMs
  when (at - r.typingSentAt > typingThrottle) do
    inRoom _ { typingSentAt = at }
    { author } <- H.get
    void $ liftAff $ Rpc.run $ r.room.typing author

-- | Run an upload, then attach what came back.
upload :: forall m. MonadAff m => Effect (Promise (Array Int)) -> App m Unit
upload go = do
  inRoom _ { uploading = true, error = Nothing }
  liftAff (attempt $ toAffE go) >>= either
    (\err -> inRoom _ { uploading = false, error = Just $ "Upload failed: " <> message err })
    (handleAction <<< Attached)

enterFromUrl :: forall m. MonadAff m => App m Unit
enterFromUrl = do
  fragment <- liftEffect $ drop 1 <$> (Location.hash =<< location)
  traverse_ (handleAction <<< Enter) $ Chat.parseRoomId chat fragment

focusComposer :: forall m. MonadAff m => App m Unit
focusComposer = H.getHTMLElementRef composerRef >>= traverse_ (liftEffect <<< focus)

withMessages :: forall a m. MonadAff m => (HTMLElement -> Effect a) -> App m (Maybe a)
withMessages act = H.getHTMLElementRef messagesRef >>= traverse (liftEffect <<< act)

nowMs :: Effect Number
nowMs = unwrap <<< unInstant <$> now

location :: Effect Location.Location
location = Window.location =<< window

localStorage :: Effect Storage.Storage
localStorage = Window.localStorage =<< window

authorKey :: String
authorKey = "chat.author"

byId :: Array Message -> Map Int Message
byId = Map.fromFoldable <<< map \m -> Tuple m.id m

-- | Swap the word the cursor is on (the last one) for `replacement`.
replaceLastWord :: String -> String -> String
replaceLastWord replacement draft =
  joinWith " " (Array.dropEnd 1 words) <> (if length words > 1 then " " else "") <> replacement
  where
  words = split (Pattern " ") draft

-- | How long a `typing` event keeps someone in the indicator, and how often
-- | we send one while the draft changes.
typingTtl :: Number
typingTtl = 3500.0

typingThrottle :: Number
typingThrottle = 1500.0
