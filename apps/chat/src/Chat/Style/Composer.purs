module Chat.Style.Composer
  ( Styles
  , raw
  , sheet
  , styles
  ) where

import Prelude

import Chat.Style.Foundation (glass, gutters, hairline)
import UI.Style (Sheet, Style, (:=), create, on, var)
import UI.Style as Style
import UI.Theme (tokens)

type Styles =
  { footer :: Style
  , replyChip :: Style
  , replyChipText :: Style
  , attachments :: Style
  , attachment :: Style
  , thumbnail :: Style
  , remove :: Style
  , uploading :: Style
  , suggestions :: Style
  , suggestion :: Style
  , compactAvatar :: Style
  , composer :: Style
  , input :: Style
  , send :: Style
  , largeIcon :: Style
  }

styles :: Styles
styles =
  { footer: glass <> create
      [ "border-block-start" := hairline
      , "display" := "grid"
      , "gap" := var tokens.space2
      , "inset-block-end" := "0"
      , "padding-block" := "0.5rem calc(0.75rem + env(safe-area-inset-bottom))"
      , "padding-inline" := gutters
      , "position" := "sticky"
      , "z-index" := "2"
      ]
  , replyChip: create
      [ "align-items" := "center"
      , "background-color" := var tokens.tint
      , "border-radius" := var tokens.radiusSm
      , "color" := var tokens.textMuted
      , "display" := "flex"
      , "font-size" := "0.8125rem"
      , "gap" := var tokens.space2
      , "padding-block" := "0.3rem"
      , "padding-inline" := "0.6rem"
      ]
  , replyChipText: create
      [ "flex" := "1"
      , "min-inline-size" := "0"
      , "overflow" := "hidden"
      , "text-overflow" := "ellipsis"
      , "white-space" := "nowrap"
      ]
  , attachments: create
      [ "align-items" := "center"
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space3
      ]
  , attachment: create
      [ "display" := "inline-flex"
      , "position" := "relative"
      ]
  , thumbnail: create
      [ "block-size" := "3.5rem"
      , "border" := hairline
      , "border-radius" := var tokens.radiusSm
      ]
  , remove: create
      [ "block-size" := "1.5rem"
      , "box-shadow" := var tokens.shadow
      , "inline-size" := "1.5rem"
      , "inset-block-start" := "-0.5rem"
      , "inset-inline-end" := "-0.5rem"
      , "min-block-size" := "1.5rem"
      , "padding-block" := "0"
      , "padding-inline" := "0"
      , "position" := "absolute"
      ]
  , uploading: create
      [ "color" := var tokens.textMuted
      , "font-size" := "0.8125rem"
      ]
  , suggestions: create
      [ "align-items" := "center"
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space2
      ]
  , suggestion: create
      [ "background-color" := var tokens.surface
      , "border-color" := var tokens.border
      , "border-radius" := "999px"
      , "gap" := "0.4rem"
      , "padding-inline" := "0.3rem 0.75rem"
      ]
  , compactAvatar: create
      [ "block-size" := "1.3rem"
      , "font-size" := "0.6rem"
      , "inline-size" := "1.3rem"
      ]
  , composer: create
      [ "align-items" := "center"
      , "background-color" := var tokens.surface
      , "border" := hairline
      , "border-radius" := "1.5rem"
      , "box-shadow" := var tokens.shadow
      , "display" := "grid"
      , "gap" := var tokens.space1
      , "grid-template-columns" := "auto minmax(0,1fr) auto"
      , "padding-block" := "0.3rem"
      , "padding-inline" := "0.4rem 0.3rem"
      , "transition" := "border-color 120ms ease,box-shadow 120ms ease"
      , on Style.FocusWithin
          [ "border-color" := var tokens.accent
          , "box-shadow" := "0 0 0 3px " <> var tokens.accentSoft <> "," <> var tokens.shadow
          ]
      ]
  , input: create
      [ "background-color" := "transparent"
      , "border-color" := "transparent"
      , "min-block-size" := "2.5rem"
      , "min-inline-size" := "0"
      , "padding-inline" := "0.5rem"
      , on Style.FocusVisible
          [ "border-color" := "transparent"
          , "box-shadow" := "none"
          , "outline" := "none"
          ]
      ]
  , send: create
      [ "block-size" := "2.5rem"
      , "border-radius" := "999px"
      , "inline-size" := "2.5rem"
      , "min-block-size" := "2.5rem"
      , "padding-block" := "0"
      , "padding-inline" := "0"
      ]
  , largeIcon: create
      [ "block-size" := "1.2rem"
      , "inline-size" := "1.2rem"
      ]
  }

sheet :: Sheet
sheet = Style.atoms $ Style.sheetFromRecord styles

raw :: String
raw = ""
