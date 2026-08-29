module Chat.Style.Hook
  ( Hook(..)
  , property
  , selector
  ) where

import Prelude

import Halogen.HTML as HH
import UI.Core (dataAttr)

data Hook
  = Message
  | MessageActions
  | Composer
  | TypingDots

name :: Hook -> String
name = case _ of
  Message -> "message"
  MessageActions -> "message-actions"
  Composer -> "composer"
  TypingDots -> "typing-dots"

property :: forall row action. Hook -> HH.IProp row action
property = dataAttr "chat" <<< name

selector :: Hook -> String
selector hook = "[data-chat=" <> name hook <> "]"
