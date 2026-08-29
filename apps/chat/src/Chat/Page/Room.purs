module Chat.Page.Room
  ( roomView
  , typingLine
  , handle
  , enter
  , onSignal
  , leaveRoom
  , tick
  ) where

import Prelude

import Chat.Client (RoomId)
import Chat.Client as Chat
import Chat.Page.Browser (away, copyText, interval, location, nearBottom, notify, nowMs, scrollToEnd, scrollToId, setTitle)
import Chat.Page.Icons (bellIcon, linkIcon)
import Chat.Page.Shared (avatar, blank, quiet, small)
import Chat.Page.Types (Action(..), App, RoomAction(..), RoomView, State, View(..), inRoom, withRoom)
import Chat.Room (Message, RoomEvents)
import Chat.Style (styles)
import Cloudflare.Durable (Signal(..))
import Cloudflare.Durable.Rpc as Rpc
import Data.Array (elem, length, mapWithIndex, replicate)
import Data.Array as Array
import Data.Either (either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.String (joinWith)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Subscription (makeEmitter)
import Markdown as Markdown
import UI.Button as Button
import UI.Core (dataAttr)
import UI.Icon as Icon
import UI.Style (css)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.Location as Location

type ViewActions action =
  { copyLink :: action
  , enableNotifications :: action
  , changeName :: action
  , leave :: action
  }

messagesRef :: H.RefLabel
messagesRef = H.RefLabel "messages"

roomView :: forall action w. ViewActions action -> State -> RoomView -> HH.HTML w action -> HH.HTML w action -> HH.HTML w action
roomView actions state room messages composer =
  HH.main [ css styles.room, HP.ref messagesRef ]
    [ HH.header [ css styles.header ]
        [ roomTitle actions state room
        , roomPeople actions state room
        ]
    , messages
    , composer
    ]

roomTitle :: forall action w. ViewActions action -> State -> RoomView -> HH.HTML w action
roomTitle actions { notifications } room =
  HH.div [ css styles.headerGroup ] $
    [ HH.span [ css $ styles.presence <> guard room.online styles.online, ARIA.hidden "true" ] []
    , HH.h1 [ css styles.roomName ] [ HH.text "Room" ]
    , HH.code [ css styles.roomId, HP.title (Chat.printRoomId room.id) ] [ HH.text $ shortId room.id ]
    , HH.span [ css styles.count, ARIA.role "status", ARIA.live "polite" ] [ HH.text $ onlineLabel room ]
    , Button.button small [ HE.onClick \_ -> actions.copyLink ]
        [ Icon.render linkIcon, HH.text if room.copied then "Copied" else "Copy link" ]
    ] <> guard (notifications == "default")
      [ Button.button small [ HE.onClick \_ -> actions.enableNotifications ]
          [ Icon.render bellIcon, HH.text "Notify me" ]
      ]

roomPeople :: forall action w. ViewActions action -> State -> RoomView -> HH.HTML w action
roomPeople actions { author } room =
  HH.div [ css styles.headerGroup ]
    [ HH.div [ css styles.members, ARIA.label $ "People online: " <> joinWith ", " room.members ] $
        room.members # mapWithIndex \index -> avatar $ styles.member <> guard (index > 0) styles.overlap
    , Button.button (quiet { styles = styles.identity }) [ HP.title "Change name", HE.onClick \_ -> actions.changeName ]
        [ avatar mempty author, HH.span_ [ HH.text author ] ]
    , Button.button quiet [ HE.onClick \_ -> actions.leave ] [ HH.text "Leave" ]
    ]

onlineLabel :: RoomView -> String
onlineLabel room
  | not room.online = "connecting…"
  | otherwise = case length room.members of
      1 -> "just you"
      count -> show count <> " online"

typingLine :: forall action w. RoomView -> HH.HTML w action
typingLine room =
  HH.div
    [ css $ styles.typing <> guard (not $ Map.isEmpty room.typing) styles.typingVisible
    , ARIA.role "status"
    , ARIA.live "polite"
    , ARIA.atomic "true"
    ]
    [ HH.span [ css styles.dots, ARIA.hidden "true", dataAttr "ui" "dots" ] $ replicate 3 $ HH.i [ css styles.dot ] []
    , HH.span_ [ HH.text $ who $ Array.fromFoldable $ Map.keys room.typing ]
    ]
  where
  who = case _ of
    [] -> ""
    [ name ] -> name <> " is typing"
    [ first, second ] -> first <> " and " <> second <> " are typing"
    _ -> "several people are typing"

handle :: forall m. MonadAff m => RoomAction -> App m Unit
handle = case _ of
  Leave -> do
    liftEffect $ Location.setHash "" =<< location
    leaveRoom \_ state -> state { view = Lobby { busy: false } }
  CopyLink -> withRoom \room -> liftEffect (copyText room.link) *> inRoom _ { copied = true }
  JumpTo id -> liftEffect $ scrollToId $ "msg-" <> show id
  React id emoji -> withRoom \room -> do
    { author } <- H.get
    liftAff (Rpc.run $ room.room.react { id, emoji, by: author }) >>= either
      (\failure -> inRoom _ { error = Just $ Chat.describeFailure failure })
      (\message -> inRoom \view -> view { messages = Map.insert message.id message view.messages })

enter :: forall m. MonadAff m => App m Unit -> RoomId -> App m Unit
enter focusComposer id = do
  liftEffect $ Location.setHash (Chat.printRoomId id) =<< location
  { author } <- H.get
  if blank author then H.modify_ _ { view = Joining { id, name: "" } }
  else do
    link <- liftEffect $ Location.href =<< location
    feed <- H.subscribe $ Notified <$> Chat.listen Chat.rpc id author
    ticker <- H.subscribe $ Tick <$ makeEmitter (interval 1000)
    H.modify_ _
      { view = InRoom
          { id
          , room: Chat.open Chat.rpc id
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
leaveRoom next = withRoom \room -> do
  H.unsubscribe room.feed
  H.unsubscribe room.ticker
  H.modify_ $ next room

onSignal :: forall m. MonadAff m => Signal (Variant RoomEvents) -> App m Unit
onSignal = case _ of
  Opened -> inRoom _ { online = true, error = Nothing } *> reload
  Closed -> inRoom _ { online = false }
  Garbled why -> inRoom _ { error = Just $ "Unreadable event: " <> why }
  Delivered event -> event # match
    { message: onMessage
    , updated: \message -> inRoom \room -> room { messages = Map.insert message.id message room.messages }
    , joined: \_ -> reload
    , left: \name -> inRoom (\room -> room { typing = Map.delete name room.typing }) *> reload
    , typing: \name -> do
        { author } <- H.get
        at <- liftEffect nowMs
        when (name /= author) $ inRoom \room -> room { typing = Map.insert name at room.typing }
    }

onMessage :: forall m. MonadAff m => Message -> App m Unit
onMessage message = do
  pinned <- withMessages nearBottom
  inRoom \room -> room { messages = Map.insert message.id message room.messages, typing = Map.delete message.author room.typing }
  { author } <- H.get
  when (fromMaybe true pinned || message.author == author) $ void $ withMessages scrollToEnd
  when (message.author /= author) $ announce message (author `elem` message.mentions)

announce :: forall m. MonadAff m => Message -> Boolean -> App m Unit
announce message mentioned = do
  gone <- liftEffect away
  when (gone || mentioned) do
    inRoom \room -> room { unread = room.unread + 1 }
    state <- H.get
    withRoom \room -> liftEffect do
      when gone $ setTitle $ "(" <> show room.unread <> ") Chat"
      when (state.notifications == "granted") $ notify
        { title: (if mentioned then "@" <> state.author <> " · " else "") <> message.author
        , body: String.take 200 (Markdown.plain message.text)
        , tag: "room-" <> Chat.printRoomId room.id
        }

reload :: forall m. MonadAff m => App m Unit
reload = withRoom \room -> do
  outcome <- liftAff $ Rpc.run do
    messages <- Rpc.infallible $ room.room.history unit
    members <- room.room.members unit
    pure { messages, members }
  either (\failure -> inRoom _ { error = Just $ Chat.describeFailure failure }) loaded outcome

loaded :: forall m. MonadAff m => { messages :: Array Message, members :: Array String } -> App m Unit
loaded { messages, members } = do
  inRoom \room -> room { messages = Map.union room.messages (byId messages), members = members }
  void $ withMessages scrollToEnd

tick :: forall m. MonadAff m => App m Unit
tick = do
  at <- liftEffect nowMs
  inRoom \room -> room { typing = Map.filter (\seen -> at - seen < typingTtl) room.typing }
  here <- liftEffect $ not <$> away
  when here $ inRoom _ { unread = 0 } *> liftEffect (setTitle "Chat")

withMessages :: forall a m. MonadAff m => (HTMLElement -> Effect a) -> App m (Maybe a)
withMessages action = H.getHTMLElementRef messagesRef >>= traverse (liftEffect <<< action)

byId :: Array Message -> Map.Map Int Message
byId = Map.fromFoldable <<< map \message -> Tuple message.id message

shortId :: RoomId -> String
shortId id = String.take 6 printed <> "…" <> String.drop (String.length printed - 4) printed
  where
  printed = Chat.printRoomId id

typingTtl :: Number
typingTtl = 3500.0
