module Chat.Page.Avatar
  ( avatar
  ) where

import Prelude

import Chat.Room (assistantName)
import Data.String as String
import Halogen.HTML as HH
import UI.Avatar as Avatar
import UI.Style (Style)

avatar :: forall widget action. Style -> String -> HH.HTML widget action
avatar extra name = Avatar.avatar
  { fallback: if name == assistantName then "✦" else String.toUpper $ String.take 1 name
  , hue: Avatar.hue name
  , styles: extra
  }
