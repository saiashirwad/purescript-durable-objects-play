module Chat.Page.Browser
  ( NotificationPermission(..)
  , TimeFormatter
  , timeFormatter
  , nearBottom
  , scrollToEnd
  , scrollToId
  , copyText
  , interval
  , away
  , notify
  , notificationPermission
  , requestNotifications
  , setTitle
  , nowMs
  , location
  , localStorage
  ) where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Promise (Promise, toAffE)
import Data.DateTime.Instant (instant, unInstant)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Timer (clearInterval, setInterval)
import JS.Intl.DateTimeFormat as DateTimeFormat
import Promise.Aff as Promise
import Web.Clipboard (clipboard, writeText)
import Web.DOM.Element (Element, clientHeight, scrollHeight, scrollTop, setScrollTop)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode, visibilityState)
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState(..))
import Web.HTML.HTMLElement (HTMLElement, toElement)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

data NotificationPermission
  = Default
  | Granted
  | Denied
  | Unsupported

derive instance Eq NotificationPermission

type TimeFormatter = Number -> String

-- | Make a locale clock formatter once for the page.
timeFormatter :: Effect TimeFormatter
timeFormatter = do
  clock <- DateTimeFormat.new [] { hour: "2-digit", minute: "2-digit" }
  pure \ms -> maybe "" (DateTimeFormat.format clock) (instant (Milliseconds ms))

nearBottom :: HTMLElement -> Effect Boolean
nearBottom el = do
  height <- scrollHeight element
  top <- scrollTop element
  visible <- clientHeight element
  pure $ height - top - visible < 96.0
  where
  element = toElement el

scrollToEnd :: HTMLElement -> Effect Unit
scrollToEnd el = scrollHeight element >>= flip setScrollTop element
  where
  element = toElement el

-- | Scroll a message into view and flash its outline.
scrollToId :: String -> Effect Unit
scrollToId id = do
  document <- toNonElementParentNode <$> (Window.document =<< window)
  getElementById id document >>= traverse_ reveal

copyText :: String -> Aff Boolean
copyText text = do
  navigator <- liftEffect $ Window.navigator =<< window
  target <- liftEffect $ clipboard navigator
  case target of
    Nothing -> pure false
    Just board -> catchError
      (Promise.toAffE (writeText text board) $> true)
      (const $ pure false)

-- | Call `push` every `ms`; the result stops it.
interval :: Int -> (Unit -> Effect Unit) -> Effect (Effect Unit)
interval ms push = clearInterval <$> setInterval ms (push unit)

-- | True when the user is not looking at this tab.
away :: Effect Boolean
away = do
  document <- Window.document =<< window
  visibility <- visibilityState document
  focused <- hasFocus
  pure $ visibility /= Visible || not focused

setTitle :: String -> Effect Unit
setTitle title = Document.setTitle title =<< Window.document =<< window

nowMs :: Effect Number
nowMs = unwrap <<< unInstant <$> now

location :: Effect Location.Location
location = Window.location =<< window

localStorage :: Effect Storage.Storage
localStorage = Window.localStorage =<< window

-- What no PureScript library covers yet: Notifications, `document.hasFocus`,
-- `scrollIntoView` with options, and the Web Animations API.
foreign import notify :: { title :: String, body :: String, tag :: String } -> Effect Unit
foreign import notificationPermissionRaw :: Effect String
foreign import requestNotificationsRaw :: Effect (Promise String)
foreign import hasFocus :: Effect Boolean
foreign import reveal :: Element -> Effect Unit

notificationPermission :: Effect NotificationPermission
notificationPermission = decodePermission <$> notificationPermissionRaw

requestNotifications :: Aff NotificationPermission
requestNotifications = decodePermission <$> toAffE requestNotificationsRaw

decodePermission :: String -> NotificationPermission
decodePermission = case _ of
  "default" -> Default
  "granted" -> Granted
  "denied" -> Denied
  _ -> Unsupported
