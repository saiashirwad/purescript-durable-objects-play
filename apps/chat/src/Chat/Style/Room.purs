module Chat.Style.Room
  ( Styles
  , raw
  , sheet
  , styles
  ) where

import Prelude

import Chat.Style.Foundation (glass, gutters, hairline)
import UI.Style (Sheet, Style, (:=), create, var)
import UI.Style as Style
import UI.Theme (tokens)

type Styles =
  { room :: Style
  , header :: Style
  , headerGroup :: Style
  , presence :: Style
  , online :: Style
  , roomName :: Style
  , roomId :: Style
  , count :: Style
  , members :: Style
  , member :: Style
  , overlap :: Style
  , identity :: Style
  , typing :: Style
  , typingVisible :: Style
  , dots :: Style
  , dot :: Style
  }

styles :: Styles
styles =
  { room: create
      [ "block-size" := "100dvh"
      , "display" := "flex"
      , "flex-direction" := "column"
      , "overflow-y" := "auto"
      , "overscroll-behavior" := "contain"
      , "scroll-behavior" := "smooth"
      ]
  , header: glass <> create
      [ "align-items" := "center"
      , "border-block-end" := hairline
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space3
      , "inset-block-start" := "0"
      , "justify-content" := "space-between"
      , "padding-block" := "0.625rem"
      , "padding-inline" := gutters
      , "position" := "sticky"
      , "z-index" := "2"
      ]
  , headerGroup: create
      [ "align-items" := "center"
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space2
      , "min-inline-size" := "0"
      ]
  , presence: create
      [ "background-color" := var tokens.textMuted
      , "block-size" := "0.5rem"
      , "border-radius" := "50%"
      , "inline-size" := "0.5rem"
      , "opacity" := "0.45"
      , "transition" := "background-color 200ms ease,box-shadow 200ms ease,opacity 200ms ease"
      ]
  , online: create
      [ "background-color" := var tokens.success
      , "box-shadow" := "0 0 0 3px color-mix(in oklab," <> var tokens.success <> " 25%,transparent)"
      , "opacity" := "1"
      ]
  , roomName: create
      [ "font-size" := "1rem"
      , "font-weight" := "600"
      , "letter-spacing" := "-0.01em"
      , "margin" := "0"
      ]
  , roomId: create
      [ "background-color" := var tokens.tint
      , "border-radius" := var tokens.radiusSm
      , "color" := var tokens.textMuted
      , "font-family" := var tokens.fontMono
      , "font-size" := "0.75rem"
      , "padding-block" := "0.15rem"
      , "padding-inline" := "0.4rem"
      ]
  , count: create
      [ "color" := var tokens.textMuted
      , "font-size" := "0.8125rem"
      ]
  , members: create
      [ "align-items" := "center"
      , "display" := "flex"
      ]
  , member: create
      [ "block-size" := "1.5rem"
      , "box-shadow" := "0 0 0 2px " <> var tokens.surface
      , "font-size" := "0.6rem"
      , "inline-size" := "1.5rem"
      ]
  , overlap: "margin-inline-start" := "-0.4rem"
  , identity: create
      [ "border-radius" := "999px"
      , "padding-inline" := "0.25rem 0.75rem"
      ]
  , typing: create
      [ "align-items" := "center"
      , "color" := var tokens.textMuted
      , "display" := "flex"
      , "font-size" := "0.8125rem"
      , "gap" := var tokens.space2
      , "min-block-size" := "1.25rem"
      , "visibility" := "hidden"
      ]
  , typingVisible: "visibility" := "visible"
  , dots: create
      [ "display" := "inline-flex"
      , "gap" := "0.2rem"
      ]
  , dot: create
      [ "animation" := "blink 1.2s infinite ease-in-out"
      , "background-color" := var tokens.textMuted
      , "block-size" := "0.35rem"
      , "border-radius" := "50%"
      , "inline-size" := "0.35rem"
      ]
  }

sheet :: Sheet
sheet = Style.atoms (Style.sheetFromRecord styles) <> Style.global raw

raw :: String
raw =
  "@keyframes blink{0%,80%,100%{opacity:.25;transform:translateY(0)}40%{opacity:1;transform:translateY(-2px)}}"
    <> "[data-ui=dots] i:nth-child(2){animation-delay:.2s}[data-ui=dots] i:nth-child(3){animation-delay:.4s}"
