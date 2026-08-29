module Chat.Style
  ( css
  ) where

import Prelude

import Chat.Style.Composer as Composer
import Chat.Style.Message as Message
import Chat.Style.Room as Room
import Chat.Style.Session as Session
import UI.Style as Style

css :: String
css = Style.renderSheet $ Session.sheet <> Room.sheet <> Message.sheet <> Composer.sheet
