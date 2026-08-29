-- | Vocabulary the page slices share: string predicates, button presets,
-- | avatars, and the image URLs the room's fetch hook serves.
module Chat.Page.Shared
  ( avatar
  , blank
  , imageEndpoint
  , imageUrl
  , quiet
  , small
  ) where

import Prelude

import Chat.Client (RoomId)
import Chat.Client as Chat
import Chat.Room (ImageId, assistantName, printImageId)
import Data.String (null, trim)
import Data.String as String
import Halogen.HTML as HH
import UI.Avatar as Avatar
import UI.Button as Button
import UI.Core (Size(..), Tone(..))
import UI.Style (Style)

-- | Any of the strings a form can be left empty with.
blank :: String -> Boolean
blank = null <<< trim

-- | Button presets: small, then small and quiet.
small :: Button.Options
small = Button.defaults { size = Small }

quiet :: Button.Options
quiet = small { tone = Quiet }

avatar :: forall w action. Style -> String -> HH.HTML w action
avatar extra name = Avatar.avatar
  { fallback: if name == assistantName then "✦" else String.toUpper $ String.take 1 name
  , hue: Avatar.hue name
  , styles: extra
  }

-- | The room's fetch hook serves uploaded images by id.
imageEndpoint :: RoomId -> String
imageEndpoint id = "/rpc/Room/id/" <> Chat.printRoomId id <> "/http/image"

imageUrl :: RoomId -> ImageId -> String
imageUrl id image = imageEndpoint id <> "/" <> show (printImageId image)
