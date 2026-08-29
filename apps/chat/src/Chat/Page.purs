module Chat.Page
  ( main
  ) where

import Prelude

import Chat.Page.Composer as Composer
import Chat.Page.Browser (NotificationPermission(..), TimeFormatter, timeFormatter)
import Chat.Page.Messages as Messages
import Chat.Page.Room as Room
import Chat.Page.Session as Session
import Chat.Page.Types (Action(..), App, ComposerAction(Reply), Html, RoomAction(..), RoomToken(..), SessionAction(..), State, View(..))
import Chat.Page.Types as Types
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  formatTime <- liftEffect timeFormatter
  body <- HA.awaitBody
  runUI page formatTime body

page :: forall query m. MonadAff m => H.Component query TimeFormatter Void m
page = H.mkComponent
  { initialState: \formatTime -> { author: "", formatTime, notifications: Default, nextRoomToken: RoomToken 0, view: Loading }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just $ Types.Session Initialize }
  }

render :: forall m. State -> Html m
render state = case state.view of
  Loading -> map Types.Session Session.loadingView
  LoadFailed why -> map Types.Session $ Session.failedView why
  Locked locked -> map Types.Session $ Session.lockedView locked
  Lobby lobby -> map Types.Session $ Session.lobbyView lobby
  Joining joining -> map Types.Session $ Session.joiningView joining
  InRoom room ->
    Room.roomView
      { copyLink: Types.Room CopyLink
      , enableNotifications: Types.Session EnableNotifications
      , changeName: Types.Session ChangeName
      , leave: Types.Room Leave
      }
      { author: state.author, notifications: state.notifications }
      room
      ( Messages.messageList
          { react: \id emoji -> Types.Room $ React id emoji
          , jumpTo: Types.Room <<< JumpTo
          , reply: Types.Composer <<< Reply
          }
          state.formatTime
          state.author
          room
      )
      (map Types.Composer $ Composer.composer state.author room (Room.typingLine room))

handleAction :: forall m. MonadAff m => Action -> App m Unit
handleAction = case _ of
  Session action -> Session.handle action >>= traverse_ \id -> do
    Room.enter id
    Types.withRoom $ const Composer.focusComposer
  Room action -> Room.handle action
  Composer action -> Composer.handle action
