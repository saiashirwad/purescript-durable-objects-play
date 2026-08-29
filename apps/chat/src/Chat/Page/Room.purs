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

import Chat.Session (RoomId)
import Chat.Session as Session
import Chat.Page.Browser (NotificationPermission(..), away, copyText, interval, location, nearBottom, notify, nowMs, scrollToEnd, scrollToId, setTitle)
import Chat.Page.Icons (bellIcon, linkIcon)
import Chat.Page.Shared (avatar, blank, quiet, small)
import Chat.Page.Types (Action(..), App, ComposerStatus(..), RoomAction(..), RoomToken, RoomView, View(..), advanceRoomToken, modifyRoomAt, withRoom)
import Chat.Room (Message, MessageId, RoomEvents, printAuthor, printMessageId)
import Chat.Style.Room (Connection(..), styles)
import Chat.Style.Room as RoomStyle
import Cloudflare.Durable (Signal(..))
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

type HeaderState =
  { author :: String
  , notifications :: NotificationPermission
  }

messagesRef :: H.RefLabel
messagesRef = H.RefLabel "messages"

roomView :: forall action w. ViewActions action -> HeaderState -> RoomView -> HH.HTML w action -> HH.HTML w action -> HH.HTML w action
roomView actions state room messages composer =
  HH.main [ css styles.room, HP.ref messagesRef ]
    [ HH.header [ css styles.header ]
        [ roomTitle actions state room
        , roomPeople actions state room
        ]
    , messages
    , composer
    ]

roomTitle :: forall action w. ViewActions action -> HeaderState -> RoomView -> HH.HTML w action
roomTitle actions { notifications } room =
  HH.div [ css styles.headerGroup ] $
    [ HH.span [ css $ RoomStyle.presence (if room.online then Connected else Disconnected), ARIA.hidden "true" ] []
    , HH.h1 [ css styles.roomName ] [ HH.text "Room" ]
    , HH.code [ css styles.roomId, HP.title (Session.printRoomId room.session.id) ] [ HH.text $ shortId room.session.id ]
    , HH.span [ css styles.count, ARIA.role "status", ARIA.live "polite" ] [ HH.text $ onlineLabel room ]
    , Button.button small [ HE.onClick \_ -> actions.copyLink ]
        [ Icon.render linkIcon, HH.text if room.copied then "Copied" else "Copy link" ]
    ] <> guard (notifications == Default)
      [ Button.button small [ HE.onClick \_ -> actions.enableNotifications ]
          [ Icon.render bellIcon, HH.text "Notify me" ]
      ]

roomPeople :: forall action w. ViewActions action -> HeaderState -> RoomView -> HH.HTML w action
roomPeople actions { author } room =
  HH.div [ css styles.headerGroup ]
    [ HH.div [ css styles.members, ARIA.role "group", ARIA.label $ "People online: " <> joinWith ", " room.members ] $
        room.members # mapWithIndex \index -> avatar $ RoomStyle.member (index > 0)
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
    [ css $ RoomStyle.typing (not $ Map.isEmpty room.typing)
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
    leaveRoom $ const $ Lobby { busy: false, error: Nothing }
  CopyLink -> withRoom \room -> do
    copied <- liftAff $ copyText room.shareUrl
    when copied $ modifyRoomAt room.token _ { copied = true }
  JumpTo id -> liftEffect $ scrollToId $ "msg-" <> show (printMessageId id)
  React id emoji -> withRoom \room -> do
    { author } <- H.get
    outcome <- liftAff $ room.session.react { id, emoji, by: author }
    either
      (\failure -> modifyRoomAt room.token _ { error = Just failure })
      (\message -> modifyRoomAt room.token \view -> view { messages = Map.insert message.id message view.messages })
      outcome
  Tick token -> tick token
  Notified token signal -> onSignal token signal

enter :: forall m. MonadAff m => RoomId -> App m Unit
enter id = do
  liftEffect $ Location.setHash (Session.route id) =<< location
  state <- H.get
  if blank state.author then H.modify_ _ { view = Joining { id, name: "", error: Nothing } }
  else do
    let token = state.nextRoomToken
    H.modify_ _ { nextRoomToken = advanceRoomToken token }
    shareUrl <- liftEffect $ Location.href =<< location
    let session = Session.open id
    feed <- H.subscribe $ (Room <<< Notified token) <$> session.listen state.author
    ticker <- H.subscribe $ Room (Tick token) <$ makeEmitter (interval 1000)
    H.modify_ _
      { view = InRoom
          { token
          , session
          , shareUrl
          , composer:
              { draft: ""
              , replyTo: Nothing
              , attachments: []
              , status: Editing
              }
          , messages: Map.empty
          , feed
          , ticker
          , online: false
          , members: []
          , typing: Map.empty
          , typingSentAt: 0.0
          , unread: 0
          , error: Nothing
          , copied: false
          }
      }

leaveRoom :: forall m. MonadAff m => (RoomView -> View) -> App m Unit
leaveRoom next = withRoom \room -> do
  H.unsubscribe room.feed
  H.unsubscribe room.ticker
  H.modify_ _ { view = next room }

onSignal :: forall m. MonadAff m => RoomToken -> Signal (Variant RoomEvents) -> App m Unit
onSignal token signal = withRoomAt token $ const $ case signal of
  Opened -> modifyRoomAt token _ { online = true, error = Nothing } *> reload token
  Closed -> modifyRoomAt token _ { online = false }
  Garbled why -> modifyRoomAt token _ { error = Just $ "Unreadable event: " <> why }
  Delivered event -> event # match
    { message: onMessage token
    , updated: \message -> modifyRoomAt token \room -> room { messages = Map.insert message.id message room.messages }
    , presence: \presence -> modifyRoomAt token \room -> room
        { members = presence
        , typing = Map.filterKeys (_ `elem` presence) room.typing
        }
    , typing: \name -> do
        { author } <- H.get
        at <- liftEffect nowMs
        when (name /= author) $ modifyRoomAt token \room -> room { typing = Map.insert name at room.typing }
    }

onMessage :: forall m. MonadAff m => RoomToken -> Message -> App m Unit
onMessage token message = do
  pinned <- withMessages nearBottom
  modifyRoomAt token \room -> room
    { messages = Map.insert message.id message room.messages
    , typing = Map.delete (printAuthor message.author) room.typing
    }
  { author } <- H.get
  when (fromMaybe true pinned || printAuthor message.author == author)
    $ withRoomAt token
    $ const
    $ void
    $ withMessages scrollToEnd
  when (printAuthor message.author /= author) $ announce token message (author `elem` message.mentions)

announce :: forall m. MonadAff m => RoomToken -> Message -> Boolean -> App m Unit
announce token message mentioned = do
  gone <- liftEffect away
  when (gone || mentioned) do
    modifyRoomAt token \room -> room { unread = room.unread + 1 }
    state <- H.get
    withRoomAt token \room -> liftEffect do
      when gone $ setTitle $ "(" <> show room.unread <> ") Chat"
      when (state.notifications == Granted) $ notify
        { title: (if mentioned then "@" <> state.author <> " · " else "") <> printAuthor message.author
        , body: String.take 200 (Markdown.plain message.text)
        , tag: "room-" <> Session.printRoomId room.session.id
        }

reload :: forall m. MonadAff m => RoomToken -> App m Unit
reload token = withRoomAt token \room -> do
  pinned <- withMessages nearBottom
  outcome <- liftAff room.session.snapshot
  either
    (\failure -> modifyRoomAt token _ { error = Just failure })
    ( \snapshot -> do
        modifyRoomAt token \current -> current
          { messages = Map.union current.messages (byId snapshot.messages)
          , members = snapshot.presence
          }
        when (fromMaybe false pinned)
          $ withRoomAt token
          $ const
          $ void
          $ withMessages scrollToEnd
    )
    outcome

tick :: forall m. MonadAff m => RoomToken -> App m Unit
tick token = withRoomAt token $ const do
  at <- liftEffect nowMs
  modifyRoomAt token \room -> room { typing = Map.filter (\seen -> at - seen < typingTtl) room.typing }
  here <- liftEffect $ not <$> away
  when here $ withRoomAt token $ const do
    modifyRoomAt token _ { unread = 0 }
    liftEffect $ setTitle "Chat"

withRoomAt :: forall m. RoomToken -> (RoomView -> App m Unit) -> App m Unit
withRoomAt token use = H.get >>= \state -> case state.view of
  InRoom room | room.token == token -> use room
  _ -> pure unit

withMessages :: forall a m. MonadAff m => (HTMLElement -> Effect a) -> App m (Maybe a)
withMessages action = H.getHTMLElementRef messagesRef >>= traverse (liftEffect <<< action)

byId :: Array Message -> Map.Map MessageId Message
byId = Map.fromFoldable <<< map \message -> Tuple message.id message

shortId :: RoomId -> String
shortId id = String.take 6 printed <> "…" <> String.drop (String.length printed - 4) printed
  where
  printed = Session.printRoomId id

typingTtl :: Number
typingTtl = 3500.0
