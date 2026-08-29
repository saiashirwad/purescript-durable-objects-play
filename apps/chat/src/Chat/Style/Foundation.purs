module Chat.Style.Foundation
  ( bubbleMax
  , contentMax
  , fastMotion
  , glass
  , gutters
  , hairline
  , mutedText
  , pill
  , pillRadius
  , smallText
  , truncate
  , wash
  ) where

import Prelude

import UI.Style (Style, (:=), create, var)
import UI.Theme (tokens)

contentMax :: String
contentMax = "72rem"

bubbleMax :: String
bubbleMax = "42rem"

smallText :: String
smallText = "0.8125rem"

fastMotion :: String
fastMotion = "120ms"

pillRadius :: String
pillRadius = "999px"

hairline :: String
hairline = "1px solid " <> var tokens.border

wash :: String
wash = "color-mix(in srgb,currentColor 9%,transparent)"

gutters :: String
gutters = "max(clamp(1rem,3vw,2.5rem),calc((100% - " <> contentMax <> ") / 2))"

glass :: Style
glass = create
  [ "-webkit-backdrop-filter" := "blur(14px) saturate(1.4)"
  , "backdrop-filter" := "blur(14px) saturate(1.4)"
  , "background-color" := "color-mix(in oklab," <> var tokens.surface <> " 78%,transparent)"
  ]

pill :: Style
pill = "border-radius" := pillRadius

truncate :: Style
truncate = create
  [ "min-inline-size" := "0"
  , "overflow" := "hidden"
  , "text-overflow" := "ellipsis"
  , "white-space" := "nowrap"
  ]

mutedText :: Style
mutedText = "color" := var tokens.textMuted
