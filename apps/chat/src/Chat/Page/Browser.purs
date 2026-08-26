module Chat.Page.Browser
  ( formatTime
  , nearBottom
  , scrollToEnd
  , scrollToId
  , copyText
  , interval
  , away
  , notify
  , setTitle
  , nowMs
  , location
  , localStorage
  ) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Now (now)
import Web.HTML (window)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

foreign import formatTime :: Number -> String
foreign import nearBottom :: HTMLElement -> Effect Boolean
foreign import scrollToEnd :: HTMLElement -> Effect Unit
foreign import scrollToId :: String -> Effect Unit
foreign import copyText :: String -> Effect Unit
foreign import interval :: Int -> (Unit -> Effect Unit) -> Effect (Effect Unit)
foreign import away :: Effect Boolean
foreign import notify :: { title :: String, body :: String, tag :: String } -> Effect Unit
foreign import setTitle :: String -> Effect Unit

nowMs :: Effect Number
nowMs = unwrap <<< unInstant <$> now

location :: Effect Location.Location
location = Window.location =<< window

localStorage :: Effect Storage.Storage
localStorage = Window.localStorage =<< window
